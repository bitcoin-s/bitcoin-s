package org.bitcoins.server

import java.nio.file.{Path, Paths}

import akka.actor.ActorSystem
import akka.dispatch.Dispatchers
import akka.http.scaladsl.Http
import com.typesafe.config.ConfigFactory
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  CompactFilterDAO,
  CompactFilterHeaderDAO
}
import org.bitcoins.core.Core
import org.bitcoins.core.api.chain.db.ChainApi
import org.bitcoins.core.config.{BitcoinNetworks, MainNet, RegTest, TestNet3}
import org.bitcoins.core.util.{BitcoinSLogger, FutureUtil, NetworkUtil}
import org.bitcoins.db._
import org.bitcoins.feeprovider.BitcoinerLiveFeeRateProvider
import org.bitcoins.node._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.wallet.Wallet
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Properties

object Main extends App with BitcoinSLogger {

  private def runMain(): Unit = {
    val argsWithIndex = args.zipWithIndex

    val dataDirIndexOpt = {
      argsWithIndex.find(_._1.toLowerCase == "--datadir")
    }
    val datadirPathOpt = dataDirIndexOpt match {
      case None => None
      case Some((_, dataDirIndex)) =>
        val str = args(dataDirIndex + 1)
        val usableStr = str.replace("~", Properties.userHome)
        Some(Paths.get(usableStr))
    }

    val configIndexOpt = {
      argsWithIndex.find(_._1.toLowerCase == "--conf")
    }
    val baseConfig = configIndexOpt match {
      case None =>
        val configPath =
          datadirPathOpt.getOrElse(AppConfig.DEFAULT_BITCOIN_S_DATADIR)
        AppConfig.getBaseConfig(configPath)
      case Some((_, configIndex)) =>
        val str = args(configIndex + 1)
        val usableStr = str.replace("~", Properties.userHome)
        val path = Paths.get(usableStr)
        ConfigFactory.parseFile(path.toFile).resolve()
    }

    val configDataDir = Paths.get(
      baseConfig.getStringOrElse("bitcoin-s.datadir",
                                 AppConfig.DEFAULT_BITCOIN_S_DATADIR.toString))
    val datadirPath = datadirPathOpt.getOrElse(configDataDir)

    val networkStr = baseConfig.getString("bitcoin-s.network")
    val network = BitcoinNetworks.fromString(networkStr)

    val datadir: Path = {
      val lastDirname = network match {
        case MainNet  => "mainnet"
        case TestNet3 => "testnet3"
        case RegTest  => "regtest"
      }
      datadirPath.resolve(lastDirname)
    }

    System.setProperty("bitcoins.log.location", datadir.toAbsolutePath.toString)

    implicit val system: ActorSystem = ActorSystem("bitcoin-s", baseConfig)
    implicit val ec: ExecutionContext = system.dispatcher

    system.log.info("Akka logger started")

    implicit val conf: BitcoinSAppConfig = {
      val dataDirOverrideOpt = datadirPathOpt.map(dir =>
        ConfigFactory.parseString(s"bitcoin-s.datadir = $dir"))

      dataDirOverrideOpt match {
        case Some(dataDirOverride) =>
          BitcoinSAppConfig(datadirPath, baseConfig, dataDirOverride)
        case None =>
          BitcoinSAppConfig(datadirPath, baseConfig)
      }
    }

    val rpcPortOpt: Option[Int] = {
      val portOpt = argsWithIndex.find(_._1.toLowerCase == "--rpcport")
      portOpt.map {
        case (_, idx) => args(idx + 1).toInt
      }
    }

    val forceChainWorkRecalc: Boolean =
      args.exists(_.toLowerCase == "--force-recalc-chainwork")

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
    val configInitializedF = conf.start()

    //run chain work migration
    val chainApiF = configInitializedF.flatMap { _ =>
      runChainWorkCalc(forceChainWorkRecalc || chainConf.forceRecalcChainWork)
    }

    //get a node that isn't started
    val nodeF = configInitializedF.flatMap { _ =>
      nodeConf.createNode(peer)(chainConf, system)
    }

    //get our wallet
    val configuredWalletF = for {
      _ <- configInitializedF
      node <- nodeF
      chainApi <- chainApiF
      _ = logger.info("Initialized chain api")
      wallet <- walletConf.createHDWallet(node,
                                          chainApi,
                                          BitcoinerLiveFeeRateProvider(60),
                                          bip39PasswordOpt)
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
    val startFut = for {
      node <- configuredNodeF
      wallet <- configuredWalletF
      _ <- node.start()
      _ <- wallet.start()
      binding <- startHttpServer(node, wallet, rpcPortOpt)
      _ = {
        if (nodeConf.isSPVEnabled) {
          logger.info(s"Starting SPV node sync")
        } else if (nodeConf.isNeutrinoEnabled) {
          logger.info(s"Starting neutrino node sync")
        } else {
          logger.info(s"Starting unknown type of node sync")
        }
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
    import system.dispatcher
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
      node: Node,
      wallet: Wallet,
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
