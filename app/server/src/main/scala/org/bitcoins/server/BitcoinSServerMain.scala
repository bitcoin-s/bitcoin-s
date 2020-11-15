package org.bitcoins.server

import akka.actor.ActorSystem
import akka.dispatch.Dispatchers
import akka.http.scaladsl.Http
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models._
import org.bitcoins.core.Core
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.util.{FutureUtil, NetworkUtil}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.feeprovider.FeeProviderName._
import org.bitcoins.feeprovider.MempoolSpaceTarget.HourFeeTarget
import org.bitcoins.feeprovider._
import org.bitcoins.node._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.wallet.Wallet
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.{ExecutionContext, Future, Promise}

class BitcoinSServerMain(override val args: Array[String])
    extends BitcoinSRunner {

  override val actorSystemName = "bitcoin-s-server"

  implicit lazy val conf: BitcoinSAppConfig =
    BitcoinSAppConfig(datadirPath, baseConfig)

  def startup: Future[Unit] = {

    implicit val walletConf: WalletAppConfig = conf.walletConf
    implicit val nodeConf: NodeAppConfig = conf.nodeConf
    implicit val chainConf: ChainAppConfig = conf.chainConf
    implicit val bitcoindRpcConf: BitcoindRpcAppConfig = conf.bitcoindRpcConf

    def startBitcoinSBackend(): Future[Unit] = {
      if (nodeConf.peers.isEmpty) {
        throw new IllegalArgumentException(
          "No peers specified, unable to start node")
      }

      val peerSocket =
        NetworkUtil.parseInetSocketAddress(nodeConf.peers.head,
                                           nodeConf.network.port)
      val peer = Peer.fromSocket(peerSocket)

      //initialize the config, run migrations
      val configInitializedF = conf.start()

      //run chain work migration
      val chainApiF = configInitializedF.flatMap { _ =>
        runChainWorkCalc(forceChainWorkRecalc || chainConf.forceRecalcChainWork)
      }

      //get a node that isn't started
      val nodeF = configInitializedF.flatMap { _ =>
        nodeConf.createNode(peer, None)(chainConf, system)
      }

      //get our wallet
      val configuredWalletF = for {
        _ <- configInitializedF
        node <- nodeF
        chainApi <- chainApiF
        _ = logger.info("Initialized chain api")
        feeProvider = getFeeProviderOrElse(MempoolSpaceProvider(HourFeeTarget))
        wallet <- walletConf.createHDWallet(node, chainApi, feeProvider)
        callbacks <- createCallbacks(wallet)
        _ = nodeConf.addCallbacks(callbacks)
      } yield {
        logger.info(s"Done configuring wallet")
        wallet
      }

      //add callbacks to our uninitialized node
      val configuredNodeF = for {
        node <- nodeF
        wallet <- configuredWalletF
        initNode <- setBloomFilter(node, wallet)
      } yield {
        logger.info(s"Done configuring node")
        initNode
      }

      //start our http server now that we are synced
      for {
        node <- configuredNodeF
        wallet <- configuredWalletF
        _ <- node.start()
        _ <- wallet.start()
        chainApi <- node.chainApiFromDb()
        binding <- startHttpServer(node, chainApi, wallet, rpcPortOpt)
        _ = {
          logger.info(s"Starting ${nodeConf.nodeType.shortName} node sync")
        }
        _ = BitcoinSServer.startedFP.success(Future.successful(binding))

        _ <- node.sync()
      } yield {
        logger.info(s"Done starting Main!")
        sys.addShutdownHook {
          logger.error(s"Exiting process")

          wallet.stop()

          node
            .stop()
            .foreach(_ =>
              logger.info(s"Stopped ${nodeConf.nodeType.shortName} node"))
          system
            .terminate()
            .foreach(_ => logger.info(s"Actor system terminated"))
        }
        ()
      }
    }

    def startBitcoindBackend(): Future[Unit] = {
      val bitcoind = bitcoindRpcConf.client

      for {
        _ <- conf.start()
        _ = logger.info("Starting bitcoind")
        _ <- bitcoindRpcConf.start()
        _ = logger.info("Creating wallet")
        feeProvider = getFeeProviderOrElse(bitcoind)
        tmpWallet <- walletConf.createHDWallet(nodeApi = bitcoind,
                                               chainQueryApi = bitcoind,
                                               feeRateApi = feeProvider)
        wallet = BitcoindRpcBackendUtil.createWalletWithBitcoindCallbacks(
          bitcoind,
          tmpWallet)
        _ = logger.info("Starting wallet")
        _ <- wallet.start()
        _ <- BitcoindRpcBackendUtil.syncWalletToBitcoind(bitcoind, wallet)

        blockCount <- bitcoind.getBlockCount
        // Create callbacks for processing new blocks
        _ = bitcoindRpcConf.zmqPortOpt match {
          case Some(_) =>
            val zmq = BitcoindRpcBackendUtil.createZMQWalletCallbacks(wallet)
            zmq.start()
          case None =>
            BitcoindRpcBackendUtil.startBitcoindBlockPolling(wallet,
                                                             bitcoind,
                                                             blockCount)
        }

        binding <- startHttpServer(bitcoind, bitcoind, wallet, rpcPortOpt)
        _ = BitcoinSServer.startedFP.success(Future.successful(binding))
      } yield {
        logger.info(s"Done starting Main!")
        sys.addShutdownHook {
          logger.error(s"Exiting process")

          wallet.stop()

          system
            .terminate()
            .foreach(_ => logger.info(s"Actor system terminated"))
        }
        ()
      }
    }

    val startFut = nodeConf.nodeType match {
      case _: InternalImplementationNodeType =>
        startBitcoinSBackend()
      case NodeType.BitcoindBackend =>
        startBitcoindBackend()
    }

    startFut.failed.foreach { err =>
      logger.error(s"Error on server startup!", err)
      err.printStackTrace()
      throw err
    }
    startFut
  }

  private def createCallbacks(wallet: Wallet)(implicit
      nodeConf: NodeAppConfig,
      ec: ExecutionContext): Future[NodeCallbacks] = {
    lazy val onTx: OnTxReceived = { tx =>
      wallet.processTransaction(tx, blockHashOpt = None).map(_ => ())
    }
    lazy val onCompactFilters: OnCompactFiltersReceived = { blockFilters =>
      wallet
        .processCompactFilters(blockFilters = blockFilters)
        .map(_ => ())
    }
    lazy val onBlock: OnBlockReceived = { block =>
      wallet.processBlock(block).map(_ => ())
    }
    lazy val onHeaders: OnBlockHeadersReceived = { headers =>
      if (headers.isEmpty) {
        FutureUtil.unit
      } else {
        wallet.updateUtxoPendingStates().map(_ => ())
      }
    }
    nodeConf.nodeType match {
      case NodeType.SpvNode =>
        Future.successful(
          NodeCallbacks(onTxReceived = Vector(onTx),
                        onBlockHeadersReceived = Vector(onHeaders)))
      case NodeType.NeutrinoNode =>
        Future.successful(
          NodeCallbacks(onBlockReceived = Vector(onBlock),
                        onCompactFiltersReceived = Vector(onCompactFilters),
                        onBlockHeadersReceived = Vector(onHeaders)))
      case NodeType.FullNode =>
        Future.failed(new RuntimeException("Not yet implemented"))
      case _: ExternalImplementationNodeType =>
        Future.failed(
          new RuntimeException(
            "Cannot create callbacks for an external implementation"))
    }
  }

  private def setBloomFilter(node: Node, wallet: Wallet)(implicit
      ec: ExecutionContext): Future[Node] = {
    for {
      nodeWithBloomFilter <- node match {
        case spvNode: SpvNode =>
          for {
            bloom <- wallet.getBloomFilter()
            _ = logger.info(
              s"Got bloom filter with ${bloom.filterSize.toInt} elements")
          } yield spvNode.setBloomFilter(bloom)
        case _: Node => Future.successful(node)
      }
    } yield {
      nodeWithBloomFilter
    }
  }

  /** This is needed for migrations V2/V3 on the chain project to re-calculate the total work for the chain */
  private def runChainWorkCalc(force: Boolean)(implicit
      chainAppConfig: ChainAppConfig,
      system: ActorSystem): Future[ChainApi] = {
    val blockEC =
      system.dispatchers.lookup(Dispatchers.DefaultBlockingDispatcherId)
    for {
      chainApi <- ChainHandler.fromDatabase(
        blockHeaderDAO = BlockHeaderDAO()(blockEC, chainAppConfig),
        CompactFilterHeaderDAO()(blockEC, chainAppConfig),
        CompactFilterDAO()(blockEC, chainAppConfig))
      isMissingChainWork <- chainApi.isMissingChainWork
      chainApiWithWork <-
        if (isMissingChainWork || force) {
          chainApi.recalculateChainWork
        } else {
          logger.info(s"Chain work already calculated")
          Future.successful(chainApi)
        }
    } yield chainApiWithWork
  }

  private def startHttpServer(
      nodeApi: NodeApi,
      chainApi: ChainApi,
      wallet: Wallet,
      rpcPortOpt: Option[Int])(implicit
      system: ActorSystem,
      conf: BitcoinSAppConfig): Future[Http.ServerBinding] = {
    implicit val nodeConf: NodeAppConfig = conf.nodeConf
    implicit val walletConf: WalletAppConfig = conf.walletConf

    val walletRoutes = WalletRoutes(wallet)
    val nodeRoutes = NodeRoutes(nodeApi)
    val chainRoutes = ChainRoutes(chainApi)
    val coreRoutes = CoreRoutes(Core)
    val server = {
      rpcPortOpt match {
        case Some(rpcport) =>
          Server(nodeConf,
                 Seq(walletRoutes, nodeRoutes, chainRoutes, coreRoutes),
                 rpcport = rpcport)
        case None =>
          conf.rpcPortOpt match {
            case Some(rpcport) =>
              Server(nodeConf,
                     Seq(walletRoutes, nodeRoutes, chainRoutes, coreRoutes),
                     rpcport)
            case None =>
              Server(nodeConf,
                     Seq(walletRoutes, nodeRoutes, chainRoutes, coreRoutes))
          }
      }
    }
    server.start()
  }

  /** Gets a Fee Provider from the given wallet app config
    * Returns default if there is no config set
    */
  def getFeeProviderOrElse(default: => FeeRateApi)(implicit
      system: ActorSystem,
      walletConf: WalletAppConfig): FeeRateApi = {
    val feeProviderNameOpt =
      walletConf.feeProviderNameOpt.flatMap(FeeProviderName.fromStringOpt)
    val feeProvider =
      (feeProviderNameOpt, walletConf.feeProviderTargetOpt) match {
        case (None, None) | (None, Some(_)) =>
          default
        case (Some(BitcoinerLive), None) =>
          BitcoinerLiveFeeRateProvider.fromBlockTarget(6)
        case (Some(BitcoinerLive), Some(target)) =>
          BitcoinerLiveFeeRateProvider.fromBlockTarget(target)
        case (Some(BitGo), targetOpt) =>
          BitGoFeeRateProvider(targetOpt)
        case (Some(MempoolSpace), None) =>
          MempoolSpaceProvider(HourFeeTarget)
        case (Some(MempoolSpace), Some(target)) =>
          MempoolSpaceProvider.fromBlockTarget(target)
        case (Some(Constant), Some(num)) =>
          ConstantFeeRateProvider(SatoshisPerVirtualByte.fromLong(num))
        case (Some(Constant), None) =>
          throw new IllegalArgumentException(
            "Missing a target for a ConstantFeeRateProvider")
      }

    logger.info(s"Using fee provider: $feeProvider")
    feeProvider
  }
}

object BitcoinSServerMain extends App {
  new BitcoinSServerMain(args).run()
}

object BitcoinSServer {
  private[server] val startedFP = Promise[Future[Http.ServerBinding]]()

  /** Allows the above server to be bundled with other projects.
    *
    * Main.startFut will be null when called from elsewhere due
    * to the delayed initialization of scala Apps.
    *
    * In contrast this Future will be initialized immediately and
    * only initialized to to Main's startFut when that future has
    * been initialized.
    */
  val startedF: Future[Http.ServerBinding] = startedFP.future.flatten
}
