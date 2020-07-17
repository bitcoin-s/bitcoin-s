package org.bitcoins.server

import java.nio.file.Paths

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  CompactFilterDAO,
  CompactFilterHeaderDAO
}
import org.bitcoins.core.Core
import org.bitcoins.core.util.{BitcoinSLogger, FutureUtil, NetworkUtil}
import org.bitcoins.db.AppConfig
import org.bitcoins.feeprovider.BitcoinerLiveFeeRateProvider
import org.bitcoins.node._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.{ExecutionContext, Future, Promise}

object Main extends App with BitcoinSLogger {

  private def runMain(): Unit = {
    implicit val system = ActorSystem("bitcoin-s")
    implicit val ec: ExecutionContext = system.dispatcher
    val argsWithIndex = args.zipWithIndex

    implicit val conf: BitcoinSAppConfig = {

      val dataDirIndexOpt = {
        argsWithIndex.find(_._1.toLowerCase == "--datadir")
      }
      val datadirPath = dataDirIndexOpt match {
        case None => AppConfig.DEFAULT_BITCOIN_S_DATADIR
        case Some((_, dataDirIndex)) =>
          val str = args(dataDirIndex + 1)
          Paths.get(str)
      }
      BitcoinSAppConfig(datadirPath)
    }

    val rpcPortOpt: Option[Int] = {
      val portOpt = argsWithIndex.find(_._1.toLowerCase == "--rpcport")
      portOpt.map {
        case (_, idx) => args(idx + 1).toInt
      }
    }
    val logger = HttpLoggerImpl(conf.nodeConf).getLogger

    implicit val walletConf: WalletAppConfig = conf.walletConf
    implicit val nodeConf: NodeAppConfig = conf.nodeConf
    require(nodeConf.isNeutrinoEnabled != nodeConf.isSPVEnabled,
            "Either Neutrino or SPV mode should be enabled")
    implicit val chainConf: ChainAppConfig = conf.chainConf

    if (nodeConf.peers.isEmpty) {
      throw new IllegalArgumentException(
        "No peers specified, unable to start node")
    }

    val peerSocket =
      NetworkUtil.parseInetSocketAddress(nodeConf.peers.head,
                                         nodeConf.network.port)
    val peer = Peer.fromSocket(peerSocket)
    val bip39PasswordOpt = None //todo need to prompt user for this

    //initialize the config, run migrations
    val configInitializedF = conf.initialize()

    //run chainwork migration
    val chainApiF = configInitializedF.flatMap { _ =>
      runChainWorkCalc()
    }

    //get a node that isn't started
    val uninitializedNodeF = configInitializedF.flatMap { _ =>
      nodeConf.createNode(peer)(chainConf, system)
    }

    //get our wallet
    val walletF = for {
      _ <- configInitializedF
      uninitializedNode <- uninitializedNodeF
      chainApi <- chainApiF
      _ = logger.info("Initialized chain api")
      wallet <- walletConf.createWallet(uninitializedNode,
                                        chainApi,
                                        BitcoinerLiveFeeRateProvider(60),
                                        bip39PasswordOpt)
      _ = logger.info("Starting wallet")
      _ <- wallet.start()
    } yield wallet

    //add callbacks to our uninitialized node
    val nodeWithCallbacksF = for {
      uninitializedNode <- uninitializedNodeF
      wallet <- walletF
      initNode <- addCallbacksAndBloomFilterToNode(uninitializedNode, wallet)
    } yield initNode

    //start and sync our node
    val startedNodeF = for {
      node <- nodeWithCallbacksF
      _ <- node.start()
    } yield node

    //start our http server now that we are synced
    val startFut = for {
      node <- startedNodeF
      wallet <- walletF
      binding <- startHttpServer(node, wallet, rpcPortOpt)

      _ =
        if (nodeConf.isSPVEnabled) {
          logger.info(s"Starting SPV node sync")
        } else if (nodeConf.isNeutrinoEnabled) {
          logger.info(s"Starting neutrino node sync")
        } else {
          logger.info(s"Starting unknown type of node sync")
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
            if (nodeConf.isSPVEnabled) {
              logger.info(s"Stopped SPV node")
            } else if (nodeConf.isNeutrinoEnabled) {
              logger.info(s"Stopped neutrino node")
            } else {
              logger.info(s"Stopped unknown type of node")
            })
        system.terminate().foreach(_ => logger.info(s"Actor system terminated"))
      }

      binding
    }
    startFut.failed.foreach { err =>
      logger.error(s"Error on server startup!", err)
    }
  }

  //start everything!
  runMain()

  private def createCallbacks(wallet: WalletApi)(implicit
      nodeConf: NodeAppConfig,
      ec: ExecutionContext): Future[NodeCallbacks] = {
    lazy val onTx: OnTxReceived = { tx =>
      wallet.processTransaction(tx, blockHash = None).map(_ => ())
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
        wallet.updateUtxoPendingStates(headers.last).map(_ => ())
      }
    }
    if (nodeConf.isSPVEnabled) {
      Future.successful(
        NodeCallbacks(onTxReceived = Vector(onTx),
                      onBlockHeadersReceived = Vector(onHeaders)))
    } else if (nodeConf.isNeutrinoEnabled) {
      Future.successful(
        NodeCallbacks(onBlockReceived = Vector(onBlock),
                      onCompactFiltersReceived = Vector(onCompactFilters),
                      onBlockHeadersReceived = Vector(onHeaders)))
    } else {
      Future.failed(new RuntimeException("Unexpected node type"))
    }
  }

  private def addCallbacksAndBloomFilterToNode(node: Node, wallet: WalletApi)(
      implicit
      nodeAppConfig: NodeAppConfig,
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
      callbacks <- createCallbacks(wallet)
    } yield {
      nodeWithBloomFilter.addCallbacks(callbacks)
    }
  }

  /** This is needed for migrations V2/V3 on the chain project to re-calculate the total work for the chain */
  private def runChainWorkCalc()(implicit
      chainAppConfig: ChainAppConfig,
      ec: ExecutionContext): Future[ChainApi] = {
    for {
      chainApi <- ChainHandler.fromDatabase(blockHeaderDAO = BlockHeaderDAO(),
                                            CompactFilterHeaderDAO(),
                                            CompactFilterDAO())
      isMissingChainWork <- chainApi.isMissingChainWork
      chainApiWithWork <-
        if (isMissingChainWork) {
          chainApi.recalculateChainWork
        } else {
          logger.info(s"Chain work already calculated")
          Future.successful(chainApi)
        }
    } yield chainApiWithWork
  }

  private def startHttpServer(
      node: Node,
      wallet: WalletApi,
      rpcPortOpt: Option[Int])(implicit
      system: ActorSystem,
      conf: BitcoinSAppConfig): Future[Http.ServerBinding] = {
    import system.dispatcher
    implicit val nodeConf: NodeAppConfig = conf.nodeConf
    for {
      syncedChainApi <- node.chainApiFromDb()
      walletRoutes = WalletRoutes(wallet, node)
      nodeRoutes = NodeRoutes(node)
      chainRoutes = ChainRoutes(syncedChainApi)
      coreRoutes = CoreRoutes(Core)
      server = {
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
      start <- server.start()
    } yield {
      start
    }
  }
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
