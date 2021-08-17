package org.bitcoins.server

import akka.actor.ActorSystem
import akka.dispatch.Dispatchers
import akka.http.scaladsl.Http
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models._
import org.bitcoins.commons.util.{DatadirParser, ServerArgParser}
import org.bitcoins.core.Core
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.dlc.node.DLCNode
import org.bitcoins.dlc.node.config.DLCNodeAppConfig
import org.bitcoins.dlc.wallet._
import org.bitcoins.feeprovider.FeeProviderName._
import org.bitcoins.feeprovider.MempoolSpaceTarget.HourFeeTarget
import org.bitcoins.feeprovider._
import org.bitcoins.node._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.rpc.config.ZmqConfig
import org.bitcoins.server.routes.{BitcoinSServerRunner, Server}
import org.bitcoins.server.util.BitcoinSAppScalaDaemon
import org.bitcoins.tor.config.TorAppConfig
import org.bitcoins.wallet.Wallet
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.{ExecutionContext, Future, Promise}

class BitcoinSServerMain(override val serverArgParser: ServerArgParser)(implicit
    override val system: ActorSystem,
    conf: BitcoinSAppConfig)
    extends BitcoinSServerRunner {

  implicit lazy val walletConf: WalletAppConfig = conf.walletConf
  implicit lazy val nodeConf: NodeAppConfig = conf.nodeConf
  implicit lazy val chainConf: ChainAppConfig = conf.chainConf
  implicit lazy val dlcConf: DLCAppConfig = conf.dlcConf
  implicit lazy val dlcNodeConf: DLCNodeAppConfig = conf.dlcNodeConf
  implicit lazy val bitcoindRpcConf: BitcoindRpcAppConfig = conf.bitcoindRpcConf
  implicit lazy val torConf: TorAppConfig = conf.torConf

  override def start(): Future[Unit] = {
    val startedConfigF = conf.start()

    logger.info(s"Start on network ${walletConf.network}")

    startedConfigF.failed.foreach { err =>
      logger.error(s"Failed to initialize configuration for BitcoinServerMain",
                   err)
    }

    for {
      _ <- startedConfigF
      start <- {
        nodeConf.nodeType match {
          case _: InternalImplementationNodeType =>
            startBitcoinSBackend()
          case NodeType.BitcoindBackend =>
            startBitcoindBackend()
        }

      }
    } yield start
  }

  override def stop(): Future[Unit] = {
    logger.error(s"Exiting process")
    for {
      _ <- walletConf.stop()
      _ <- nodeConf.stop()
      _ <- chainConf.stop()
      _ <- torConf.stop()
      _ = logger.info(s"Stopped ${nodeConf.nodeType.shortName} node")
      _ <- system.terminate()
    } yield {
      logger.info(s"Actor system terminated")
      ()
    }
  }

  def startBitcoinSBackend(): Future[Unit] = {
    val start = System.currentTimeMillis()
    if (nodeConf.peers.isEmpty) {
      throw new IllegalArgumentException(
        "No peers specified, unable to start node")
    }

    val peerSockets = {
      nodeConf.peers.map(
        NetworkUtil.parseInetSocketAddress(_, nodeConf.network.port)
      )
    }

    val peers = peerSockets.map(Peer.fromSocket(_, nodeConf.socks5ProxyParams))

    //run chain work migration
    val chainApiF = runChainWorkCalc(
      serverArgParser.forceChainWorkRecalc || chainConf.forceRecalcChainWork)

    //get a node that isn't started
    val nodeF = nodeConf.createNode(peers)(chainConf, system)

    val feeProvider = getFeeProviderOrElse(
      MempoolSpaceProvider(HourFeeTarget,
                           walletConf.network,
                           walletConf.torConf.socks5ProxyParams))
    //get our wallet
    val configuredWalletF = for {
      node <- nodeF
      chainApi <- chainApiF
      _ = logger.info("Initialized chain api")
      wallet <- dlcConf.createDLCWallet(node, chainApi, feeProvider)
      callbacks <- createCallbacks(wallet)
      _ = nodeConf.addCallbacks(callbacks)
    } yield {
      logger.info(
        s"Done configuring wallet, it took=${System.currentTimeMillis() - start}ms")
      wallet
    }

    //add callbacks to our uninitialized node
    val configuredNodeF = for {
      node <- nodeF
      wallet <- configuredWalletF
      initNode <- setBloomFilter(node, wallet)
    } yield {
      logger.info(
        s"Done configuring node, it took=${System.currentTimeMillis() - start}ms")
      initNode
    }

    val dlcNodeF = for {
      wallet <- configuredWalletF
      node = dlcNodeConf.createDLCNode(wallet)
    } yield node

    //start our http server now that we are synced
    for {
      node <- configuredNodeF
      wallet <- configuredWalletF
      _ <- node.start()
      _ <- wallet.start().recoverWith {
        //https://github.com/bitcoin-s/bitcoin-s/issues/2917
        //https://github.com/bitcoin-s/bitcoin-s/pull/2918
        case err: IllegalArgumentException
            if err.getMessage.contains("If we have spent a spendinginfodb") =>
          handleMissingSpendingInfoDb(err, wallet)
      }
      cachedChainApi <- node.chainApiFromDb()
      chainApi = ChainHandler.fromChainHandlerCached(cachedChainApi)

      dlcNode <- dlcNodeF
      _ <- dlcNode.start()

      binding <- startHttpServer(nodeApi = node,
                                 chainApi = chainApi,
                                 wallet = wallet,
                                 dlcNode = dlcNode,
                                 serverCmdLineArgs = serverArgParser)
      _ = {
        logger.info(
          s"Starting ${nodeConf.nodeType.shortName} node sync, it took=${System
            .currentTimeMillis() - start}ms")
      }
      _ = BitcoinSServer.startedFP.success(Future.successful(binding))

      _ <- node.sync()
    } yield {
      logger.info(
        s"Done starting Main! It took ${System.currentTimeMillis() - start}ms")
      ()
    }
  }

  def startBitcoindBackend(): Future[Unit] = {
    val bitcoind = bitcoindRpcConf.client

    for {
      _ <- bitcoindRpcConf.start()
      _ = logger.info("Started bitcoind")

      bitcoindNetwork <- bitcoind.getBlockChainInfo.map(_.chain)
      _ = require(
        bitcoindNetwork == walletConf.network,
        s"bitcoind ($bitcoindNetwork) on different network than wallet (${walletConf.network})")

      _ = logger.info("Creating wallet")
      feeProvider = getFeeProviderOrElse(bitcoind)
      tmpWallet <- dlcConf.createDLCWallet(nodeApi = bitcoind,
                                           chainQueryApi = bitcoind,
                                           feeRateApi = feeProvider)
      wallet = BitcoindRpcBackendUtil.createDLCWalletWithBitcoindCallbacks(
        bitcoind,
        tmpWallet)
      _ = logger.info("Starting wallet")
      _ <- wallet.start().recoverWith {
        //https://github.com/bitcoin-s/bitcoin-s/issues/2917
        //https://github.com/bitcoin-s/bitcoin-s/pull/2918
        case err: IllegalArgumentException
            if err.getMessage.contains("If we have spent a spendinginfodb") =>
          handleMissingSpendingInfoDb(err, wallet)
      }
      _ = BitcoindRpcBackendUtil
        .syncWalletToBitcoind(bitcoind, wallet)
        .flatMap(_ => wallet.updateUtxoPendingStates())
        .flatMap { _ =>
          if (bitcoindRpcConf.zmqConfig == ZmqConfig.empty) {
            BitcoindRpcBackendUtil.startBitcoindBlockPolling(wallet, bitcoind)
          } else Future.unit
        }

      // Create callbacks for processing new blocks
      _ =
        if (bitcoindRpcConf.zmqConfig != ZmqConfig.empty) {
          BitcoindRpcBackendUtil.startZMQWalletCallbacks(
            wallet,
            bitcoindRpcConf.zmqConfig)
        }

      dlcNode = dlcNodeConf.createDLCNode(wallet)
      _ <- dlcNode.start()

      binding <- startHttpServer(nodeApi = bitcoind,
                                 chainApi = bitcoind,
                                 wallet = wallet,
                                 dlcNode = dlcNode,
                                 serverCmdLineArgs = serverArgParser)
      _ = BitcoinSServer.startedFP.success(Future.successful(binding))
    } yield {
      logger.info(s"Done starting Main!")
      ()
    }
  }

  private def createCallbacks(wallet: Wallet)(implicit
      nodeConf: NodeAppConfig,
      ec: ExecutionContext): Future[NodeCallbacks] = {
    lazy val onTx: OnTxReceived = { tx =>
      logger.debug(s"Receiving transaction txid=${tx.txIdBE.hex} as a callback")
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
        Future.unit
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
          NodeCallbacks(onTxReceived = Vector(onTx),
                        onBlockReceived = Vector(onBlock),
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
      system: ActorSystem): Future[ChainApi] = {
    val blockEC =
      system.dispatchers.lookup(Dispatchers.DefaultBlockingDispatcherId)
    val chainApi = ChainHandler.fromDatabase(
      blockHeaderDAO = BlockHeaderDAO()(blockEC, chainConf),
      CompactFilterHeaderDAO()(blockEC, chainConf),
      CompactFilterDAO()(blockEC, chainConf))
    for {
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
      wallet: DLCWallet,
      dlcNode: DLCNode,
      serverCmdLineArgs: ServerArgParser)(implicit
      system: ActorSystem,
      conf: BitcoinSAppConfig): Future[Http.ServerBinding] = {
    implicit val nodeConf: NodeAppConfig = conf.nodeConf
    implicit val walletConf: WalletAppConfig = conf.walletConf

    val walletRoutes = WalletRoutes(wallet)
    val nodeRoutes = NodeRoutes(nodeApi)
    val chainRoutes = ChainRoutes(chainApi, nodeConf.network)
    val coreRoutes = CoreRoutes(Core)
    val dlcRoutes = DLCRoutes(dlcNode)

    val handlers =
      Seq(walletRoutes, nodeRoutes, chainRoutes, coreRoutes, dlcRoutes)

    val bindConfOpt = serverCmdLineArgs.rpcBindOpt match {
      case Some(rpcbind) => Some(rpcbind)
      case None          => conf.rpcBindOpt
    }

    val server = {
      serverCmdLineArgs.rpcPortOpt match {
        case Some(rpcport) =>
          Server(conf = nodeConf,
                 handlers = handlers,
                 rpcbindOpt = bindConfOpt,
                 rpcport = rpcport)
        case None =>
          Server(conf = nodeConf,
                 handlers = handlers,
                 rpcbindOpt = bindConfOpt,
                 rpcport = conf.rpcPort)
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
    val proxyParams = walletConf.torConf.socks5ProxyParams
    val feeProviderNameOpt =
      walletConf.feeProviderNameOpt.flatMap(FeeProviderName.fromStringOpt)
    val feeProvider =
      (feeProviderNameOpt, walletConf.feeProviderTargetOpt) match {
        case (None, None) | (None, Some(_)) =>
          default
        case (Some(BitcoinerLive), None) =>
          BitcoinerLiveFeeRateProvider.fromBlockTarget(6, proxyParams)
        case (Some(BitcoinerLive), Some(target)) =>
          BitcoinerLiveFeeRateProvider.fromBlockTarget(target, proxyParams)
        case (Some(BitGo), targetOpt) =>
          BitGoFeeRateProvider(targetOpt, proxyParams)
        case (Some(MempoolSpace), None) =>
          MempoolSpaceProvider(HourFeeTarget, walletConf.network, proxyParams)
        case (Some(MempoolSpace), Some(target)) =>
          MempoolSpaceProvider.fromBlockTarget(target,
                                               walletConf.network,
                                               proxyParams)
        case (Some(Constant), Some(num)) =>
          ConstantFeeRateProvider(SatoshisPerVirtualByte.fromLong(num))
        case (Some(Constant), None) =>
          throw new IllegalArgumentException(
            "Missing a target for a ConstantFeeRateProvider")
      }

    logger.info(s"Using fee provider: $feeProvider")
    feeProvider
  }

  /** Handles a bug we had in our wallet with missing the spendingTxId for transactions spent from our wallet database.
    * This clears the utxos/addresses from the wallet and then
    * starts a rescan to find the missing spending txids
    * @see https://github.com/bitcoin-s/bitcoin-s/issues/2917
    * @see https://github.com/bitcoin-s/bitcoin-s/pull/2918
    */
  private def handleMissingSpendingInfoDb(err: Throwable, wallet: Wallet)(
      implicit walletConf: WalletAppConfig): Future[Unit] = {
    logger.warn(
      s"Found corrupted wallet, rescanning to find spendinginfodbs.spendingTxId as detailed in issue 2917",
      err)

    //clear the entire wallet, then rescan to make sure we get out of a corrupted state
    val clearedF = wallet.clearAllUtxosAndAddresses()
    val walletF = for {
      clearedWallet <- clearedF
      _ <- clearedWallet.rescanNeutrinoWallet(startOpt = None,
                                              endOpt = None,
                                              addressBatchSize =
                                                walletConf.discoveryBatchSize,
                                              useCreationTime = true)
    } yield clearedWallet
    walletF.map(_ => ())
  }
}

object BitcoinSServerMain extends BitcoinSAppScalaDaemon {

  override val actorSystemName =
    s"bitcoin-s-server-${System.currentTimeMillis()}"

  /** Directory specific for current network or custom dir */
  override val customFinalDirOpt: Option[String] = None

  val serverCmdLineArgs = ServerArgParser(args.toVector)

  val datadirParser =
    DatadirParser(serverCmdLineArgs, customFinalDirOpt)

  System.setProperty("bitcoins.log.location", datadirParser.networkDir.toString)

  implicit lazy val conf: BitcoinSAppConfig =
    BitcoinSAppConfig(datadirParser.datadir,
                      datadirParser.baseConfig,
                      serverCmdLineArgs.toConfig)(system.dispatcher)

  new BitcoinSServerMain(serverCmdLineArgs).run()
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
