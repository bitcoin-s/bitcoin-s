package org.bitcoins.server

import akka.actor.ActorSystem
import akka.stream.scaladsl.{
  BroadcastHub,
  Keep,
  Sink,
  Source,
  SourceQueueWithComplete
}
import akka.stream.{KillSwitches, OverflowStrategy, SharedKillSwitch}
import akka.{Done, NotUsed}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.asyncutil.AsyncUtil.Exponential
import org.bitcoins.chain.ChainCallbacks
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockChainInfoResult
import org.bitcoins.commons.jsonmodels.ws.WsNotification
import org.bitcoins.commons.util.{DatadirParser, ServerArgParser}
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.dlc.wallet.AnyDLCHDWalletApi
import org.bitcoins.core.api.node.{
  InternalImplementationNodeType,
  NodeApi,
  NodeType
}
import org.bitcoins.core.api.wallet.{NeutrinoWalletApi, WalletApi}
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.core.wallet.rescan.RescanState
import org.bitcoins.dlc.node.DLCNode
import org.bitcoins.dlc.node.config.DLCNodeAppConfig
import org.bitcoins.dlc.wallet._
import org.bitcoins.feeprovider.MempoolSpaceTarget.HourFeeTarget
import org.bitcoins.feeprovider._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.NodeStateDescriptorDAO
import org.bitcoins.rpc.BitcoindException.InWarmUp
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.{BitcoindRpcAppConfig, ZmqConfig}
import org.bitcoins.server.routes.{BitcoinSServerRunner, CommonRoutes, Server}
import org.bitcoins.server.util._
import org.bitcoins.tor.config.TorAppConfig
import org.bitcoins.wallet.WalletHolder
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.SpendingInfoDAO

import java.sql.SQLException
import java.time.Instant
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future, Promise}

class BitcoinSServerMain(override val serverArgParser: ServerArgParser)(implicit
    override val system: ActorSystem,
    val conf: BitcoinSAppConfig)
    extends BitcoinSServerRunner {

  implicit lazy val nodeConf: NodeAppConfig = conf.nodeConf
  implicit lazy val chainConf: ChainAppConfig = conf.chainConf
  implicit lazy val dlcNodeConf: DLCNodeAppConfig = conf.dlcNodeConf
  implicit lazy val bitcoindRpcConf: BitcoindRpcAppConfig = conf.bitcoindRpcConf
  implicit lazy val torConf: TorAppConfig = conf.torConf
  lazy val network = conf.walletConf.network

  private val nodeCallbackKillSwitch: SharedKillSwitch = {
    KillSwitches.shared("node-callback-killswitch")
  }

  override def start(): Future[Unit] = {
    logger.info("Starting appServer")
    val startTime = TimeUtil.currentEpochMs
    val startedConfigF = conf.start()

    logger.info(s"Start on network $network")

    startedConfigF.failed.foreach { err =>
      logger.error(s"Failed to initialize configuration for BitcoinServerMain",
                   err)
    }

    for {
      startedConfig <- startedConfigF
      chainApi = ChainHandler.fromDatabase()
      //on server startup we assume we are out of sync with the bitcoin network
      //so we set this flag to true.
      _ <- chainApi.setSyncing(true)
      start <- {
        nodeConf.nodeType match {
          case _: InternalImplementationNodeType =>
            startBitcoinSBackend(startedConfig.torStartedF)
          case NodeType.BitcoindBackend =>
            startBitcoindBackend(startedConfig.torStartedF)
        }
      }
    } yield {
      logger.info(
        s"Done start BitcoinSServerMain, it took=${TimeUtil.currentEpochMs - startTime}ms")
      start
    }
  }

  override def stop(): Future[Unit] = {
    logger.error(s"Exiting process")
    for {
      _ <- conf.stop()
      _ <- serverBindingsOpt match {
        case Some(bindings) => bindings.stop()
        case None           => Future.unit
      }
      _ = logger.info(s"Stopped ${nodeConf.nodeType.shortName} node")
    } yield {
      ()
    }
  }

  /** Start the bitcoin-s wallet server with a neutrino backend
    * @param startedTorConfigF a future that is completed when tor is fully started
    * @return
    */
  def startBitcoinSBackend(startedTorConfigF: Future[Unit]): Future[Unit] = {
    logger.info(s"startBitcoinSBackend()")
    val start = System.currentTimeMillis()

    val chainApi = ChainHandler.fromDatabase()
    val creationTime: Instant = conf.walletConf.creationTime

    //get a node that isn't started
    val nodeF = nodeConf.createNode(
      peers = Vector.empty,
      walletCreationTimeOpt = Some(creationTime))(chainConf, system)

    val defaultApi =
      MempoolSpaceProvider(HourFeeTarget, network, torConf.socks5ProxyParams)
    val feeProvider = FeeProviderFactory.getFeeProviderOrElse(
      defaultApi,
      conf.walletConf.feeProviderNameOpt,
      conf.walletConf.feeProviderTargetOpt,
      torConf.socks5ProxyParams,
      network)
    //get our wallet
    val walletHolder = new WalletHolder()
    val neutrinoWalletLoaderF = {
      for {
        node <- nodeF
      } yield {
        DLCWalletNeutrinoBackendLoader(walletHolder,
                                       chainApi,
                                       nodeApi = node,
                                       feeRateApi = feeProvider)
      }
    }

    val configuredWalletF = {
      for {
        walletNameOpt <- getLastLoadedWalletName()
        neutrinoWalletLoader <- neutrinoWalletLoaderF
        walletWithConfigs <- neutrinoWalletLoader.load(
          walletNameOpt = walletNameOpt,
          aesPasswordOpt = conf.walletConf.aesPasswordOpt)
      } yield walletWithConfigs
    }

    //add callbacks to our uninitialized node
    val configuredNodeF = for {
      node <- nodeF
      _ <- configuredWalletF
    } yield {
      logger.info(
        s"Done configuring node, it took=${System.currentTimeMillis() - start}ms")
      node
    }

    val dlcNodeF = for {
      (wallet, _, _) <- configuredWalletF
      node = dlcNodeConf.createDLCNode(wallet)
    } yield node

    val tuple = buildWsSource

    val wsQueue: SourceQueueWithComplete[WsNotification[_]] = tuple._1
    val wsSource: Source[WsNotification[_], NotUsed] = tuple._2

    val callbacksF: Future[Unit] = for {
      (_, walletConfig, dlcConfig) <- configuredWalletF
    } yield buildNeutrinoCallbacks(wsQueue, chainApi, walletConfig, dlcConfig)

    val torCallbacks = WebsocketUtil.buildTorCallbacks(wsQueue)
    torConf.addCallbacks(torCallbacks)

    val isTorStartedF = if (torConf.torProvided) {
      //if tor is provided we need to execute the tor started callback immediately
      torConf.callBacks.executeOnTorStarted()
    } else {
      Future.unit
    }
    val startedNodeF = {
      //can't start connecting to peers until tor is done starting
      for {
        _ <- startedTorConfigF
        _ <- isTorStartedF
        started <- configuredNodeF.flatMap(_.start())
      } yield started
    }

    val startedDLCNodeF = {
      for {
        dlcNode <- dlcNodeF
        _ <- dlcNode.start()
      } yield dlcNode
    }

    //start our http server now that we are synced
    for {
      neutrinoWalletLoader <- neutrinoWalletLoaderF
      _ <- startHttpServer(
        nodeApiF = startedNodeF,
        chainApi = chainApi,
        walletF = configuredWalletF.map(_._1),
        dlcNodeF = startedDLCNodeF,
        torConfStarted = startedTorConfigF,
        serverCmdLineArgs = serverArgParser,
        wsSource = wsSource,
        neutrinoWalletLoader
      )
      _ = {
        logger.info(
          s"Starting ${nodeConf.nodeType.shortName} node sync, it took=${System
            .currentTimeMillis() - start}ms")
      }
      //make sure callbacks are registered before we start sync
      _ <- callbacksF
      node <- startedNodeF
      _ <- startedTorConfigF
      _ <- node.sync()
      (wallet, walletConfig, _) <- configuredWalletF
      _ <- handleDuplicateSpendingInfoDb(wallet, walletConfig)
      _ <- restartRescanIfNeeded(wallet)
    } yield {
      logger.info(
        s"Done starting Main! It took ${System.currentTimeMillis() - start}ms")
      ()
    }
  }

  private def buildNeutrinoCallbacks(
      wsQueue: SourceQueueWithComplete[WsNotification[_]],
      chainApi: ChainApi,
      walletConf: WalletAppConfig,
      dlcConf: DLCAppConfig): Unit = {
    val chainCallbacks = WebsocketUtil.buildChainCallbacks(wsQueue, chainApi)
    chainConf.addCallbacks(chainCallbacks)

    val walletCallbacks =
      WebsocketUtil.buildWalletCallbacks(wsQueue, walletConf.walletName)
    walletConf.addCallbacks(walletCallbacks)

    val dlcWalletCallbacks = WebsocketUtil.buildDLCWalletCallbacks(wsQueue)
    dlcConf.addCallbacks(dlcWalletCallbacks)

    val torCallbacks = WebsocketUtil.buildTorCallbacks(wsQueue)
    torConf.addCallbacks(torCallbacks)

    ()
  }

  /** Returns blockchain info, in case of  [[InWarmUp]] exception it retries.
    */
  private def getBlockChainInfo(
      client: BitcoindRpcClient): Future[GetBlockChainInfoResult] = {
    val promise = Promise[GetBlockChainInfoResult]()
    for {
      _ <- AsyncUtil.retryUntilSatisfiedF(
        conditionF = { () =>
          val infoF = client.getBlockChainInfo
          val res = infoF.map(promise.success).map(_ => true)
          res.recover { case _: InWarmUp => false }
        },
        // retry for approximately 2 hours
        mode = Exponential,
        interval = 1.second,
        maxTries = 12
      )
      info <- promise.future
    } yield info
  }

  /** Start the bitcoin-s wallet server with a bitcoind backend
    * @param startedTorConfigF a future that is completed when tor is fully started
    * @return
    */
  def startBitcoindBackend(startedTorConfigF: Future[Unit]): Future[Unit] = {
    logger.info(s"startBitcoindBackend()")
    val bitcoindF = for {
      client <- bitcoindRpcConf.clientF
      _ <- client.start()
    } yield {
      logger.info("Started bitcoind")
      client
    }
    val tuple = buildWsSource
    val wsQueue: SourceQueueWithComplete[WsNotification[_]] = tuple._1
    val wsSource: Source[WsNotification[_], NotUsed] = tuple._2
    val torCallbacks = WebsocketUtil.buildTorCallbacks(wsQueue)
    val _ = torConf.addCallbacks(torCallbacks)
    val isTorStartedF = if (torConf.torProvided) {
      //if tor is provided we need to emit a tor started event immediately
      torConf.callBacks.executeOnTorStarted()
    } else {
      Future.unit
    }
    val walletNameF = for {
      lastLoadedWallet <- getLastLoadedWalletName()
      walletName = lastLoadedWallet.getOrElse(
        WalletAppConfig.DEFAULT_WALLET_NAME)
    } yield walletName

    val walletHolder = new WalletHolder()
    val chainCallbacksF = for {
      bitcoind <- bitcoindF
    } yield {
      WebsocketUtil.buildChainCallbacks(wsQueue, bitcoind)
    }
    val nodeApiF = for {
      bitcoind <- bitcoindF
      chainCallbacks <- chainCallbacksF
    } yield BitcoindRpcBackendUtil.buildBitcoindNodeApi(bitcoind,
                                                        walletHolder,
                                                        Some(chainCallbacks))

    val feeProviderF = bitcoindF.map { bitcoind =>
      FeeProviderFactory.getFeeProviderOrElse(
        bitcoind,
        feeProviderNameStrOpt = conf.walletConf.feeProviderNameOpt,
        feeProviderTargetOpt = conf.walletConf.feeProviderTargetOpt,
        proxyParamsOpt = torConf.socks5ProxyParams,
        network = network
      )
    }

    val loadWalletApiF = {
      for {
        bitcoind <- bitcoindF
        nodeApi <- nodeApiF
        feeProvider <- feeProviderF
      } yield DLCWalletBitcoindBackendLoader(walletHolder,
                                             bitcoind,
                                             nodeApi,
                                             feeProvider)
    }

    val walletF: Future[(WalletHolder, WalletAppConfig, DLCAppConfig)] = {
      for {
        _ <- isTorStartedF
        loadWalletApi <- loadWalletApiF
        walletName <- walletNameF
        result <- loadWalletApi.load(Some(walletName),
                                     conf.walletConf.aesPasswordOpt)
      } yield result
    }

    val dlcNodeF = {
      for {
        (wallet, _, _) <- walletF
        dlcNode = dlcNodeConf.createDLCNode(wallet)
        _ <- dlcNode.start()
      } yield dlcNode
    }

    for {
      bitcoind <- bitcoindF
      bitcoindNetwork <- getBlockChainInfo(bitcoind).map(_.chain)
      _ = require(
        bitcoindNetwork == network,
        s"bitcoind ($bitcoindNetwork) on different network than wallet ($network)")
      loadWalletApi <- loadWalletApiF
      _ <- startHttpServer(
        nodeApiF = Future.successful(bitcoind),
        chainApi = bitcoind,
        walletF = walletF.map(_._1),
        dlcNodeF = dlcNodeF,
        torConfStarted = startedTorConfigF,
        serverCmdLineArgs = serverArgParser,
        wsSource = wsSource,
        loadWalletApi
      )
      walletName <- walletNameF
      walletCallbacks = WebsocketUtil.buildWalletCallbacks(wsQueue, walletName)
      chainCallbacks <- chainCallbacksF
      (wallet, walletConfig, dlcConfig) <- walletF
      _ = walletConfig.addCallbacks(walletCallbacks)

      //intentionally doesn't map on this otherwise we
      //wait until we are done syncing the entire wallet
      //which could take 1 hour
      _ = syncWalletWithBitcoindAndStartPolling(bitcoind,
                                                wallet,
                                                walletConfig,
                                                Some(chainCallbacks))
      dlcWalletCallbacks = WebsocketUtil.buildDLCWalletCallbacks(wsQueue)
      _ = dlcConfig.addCallbacks(dlcWalletCallbacks)
      _ <- startedTorConfigF
      _ <- handleDuplicateSpendingInfoDb(wallet, walletConfig)
      _ <- restartRescanIfNeeded(wallet)
    } yield {
      logger.info(s"Done starting Main!")
      ()
    }
  }

  private var serverBindingsOpt: Option[ServerBindings] = None

  private def startHttpServer(
      nodeApiF: Future[NodeApi],
      chainApi: ChainApi,
      walletF: Future[WalletHolder],
      dlcNodeF: Future[DLCNode],
      torConfStarted: Future[Unit],
      serverCmdLineArgs: ServerArgParser,
      wsSource: Source[WsNotification[_], NotUsed],
      loadWalletApi: DLCWalletLoaderApi)(implicit
      system: ActorSystem,
      conf: BitcoinSAppConfig): Future[Server] = {
    implicit val nodeConf: NodeAppConfig = conf.nodeConf
    implicit val walletConf: WalletAppConfig = conf.walletConf

    val walletRoutesF = {
      walletF.map { w =>
        WalletRoutes(w)(loadWalletApi)
      }
    }
    val nodeRoutesF = nodeApiF.map(NodeRoutes(_))
    val chainRoutes =
      ChainRoutes(chainApi, nodeConf.network, torConfStarted)
    val coreRoutes = CoreRoutes()
    val dlcRoutesF = dlcNodeF.map(DLCRoutes(_))
    val commonRoutes = CommonRoutes(conf.baseDatadir)

    val handlers =
      Seq(walletRoutesF,
          nodeRoutesF,
          Future.successful(chainRoutes),
          Future.successful(coreRoutes),
          dlcRoutesF,
          Future.successful(commonRoutes))

    val rpcBindConfOpt = serverCmdLineArgs.rpcBindOpt match {
      case Some(rpcbind) => Some(rpcbind)
      case None          => conf.rpcBindOpt
    }

    val wsBindConfOpt = serverCmdLineArgs.wsBindOpt match {
      case Some(wsbind) => Some(wsbind)
      case None         => conf.wsBindOpt
    }

    val wsPort = serverCmdLineArgs.wsPortOpt match {
      case Some(wsPort) => wsPort
      case None         => conf.wsPort
    }

    val wsServerConfig =
      WsServerConfig(wsBindConfOpt.getOrElse("localhost"), wsPort = wsPort)

    val server = {
      serverCmdLineArgs.rpcPortOpt match {
        case Some(rpcport) =>
          Server(conf = nodeConf,
                 handlersF = handlers,
                 rpcbindOpt = rpcBindConfOpt,
                 rpcport = rpcport,
                 rpcPassword = conf.rpcPassword,
                 wsConfigOpt = Some(wsServerConfig),
                 wsSource)
        case None =>
          Server(
            conf = nodeConf,
            handlersF = handlers,
            rpcbindOpt = rpcBindConfOpt,
            rpcport = conf.rpcPort,
            rpcPassword = conf.rpcPassword,
            wsConfigOpt = Some(wsServerConfig),
            wsSource
          )
      }
    }
    val bindingF = server.start()

    bindingF.map { bindings =>
      serverBindingsOpt = Some(bindings)
      server
    }
  }

  /** Syncs the bitcoin-s wallet against bitcoind and then
    * starts rpc polling if zmq isn't enabled, otherwise it starts zmq polling.
    *
    * The key thing this helper method does is it logs errors based on the
    * future returned by this method. This is needed because we don't want
    * to block the rest of the application from starting if we have to
    * do a ton of syncing. However, we don't want to swallow
    * exceptions thrown by this method.
    */
  private def syncWalletWithBitcoindAndStartPolling(
      bitcoind: BitcoindRpcClient,
      wallet: WalletApi with NeutrinoWalletApi,
      walletConfig: WalletAppConfig,
      chainCallbacksOpt: Option[ChainCallbacks]): Future[Unit] = {
    val f = for {
      _ <- AsyncUtil.retryUntilSatisfiedF(
        conditionF = { () =>
          for {
            bitcoindHeight <- bitcoind.getBlockCount
            walletStateOpt <- wallet.getSyncDescriptorOpt()
          } yield walletStateOpt.forall(bitcoindHeight >= _.height)
        },
        // retry for approximately 2 hours
        mode = Exponential,
        interval = 1.second,
        maxTries = 12
      )
      _ <- BitcoindRpcBackendUtil.syncWalletToBitcoind(
        bitcoind,
        wallet,
        chainCallbacksOpt)(system, walletConfig)
      _ <- wallet.updateUtxoPendingStates()
      _ <-
        if (bitcoindRpcConf.zmqConfig == ZmqConfig.empty) {
          BitcoindRpcBackendUtil
            .startBitcoindBlockPolling(wallet, bitcoind, chainCallbacksOpt)
            .map { _ =>
              BitcoindRpcBackendUtil
                .startBitcoindMempoolPolling(wallet, bitcoind) { tx =>
                  nodeConf.callBacks
                    .executeOnTxReceivedCallbacks(logger, tx)
                }
              ()
            }
        } else {
          Future {
            BitcoindRpcBackendUtil.startZMQWalletCallbacks(
              wallet,
              bitcoindRpcConf.zmqConfig)
          }
        }
    } yield ()

    f.failed.foreach(err =>
      logger.error(s"Error syncing bitcoin-s wallet with bitcoind", err))
    f
  }

  /** Builds a websocket queue that you can feed elements to.
    * The Source can be wired up with Directives.handleWebSocketMessages
    * to create a flow that emits websocket messages
    */
  private def buildWsSource: (
      SourceQueueWithComplete[WsNotification[_]],
      Source[WsNotification[_], NotUsed]) = {
    val maxBufferSize: Int = 25

    /** This will queue [[maxBufferSize]] elements in the queue. Once the buffer size is reached,
      * we will drop the first element in the buffer
      */
    val tuple = {
      //from: https://github.com/akka/akka-http/issues/3039#issuecomment-610263181
      //the BroadcastHub.sink is needed to avoid these errors
      // 'Websocket handler failed with Processor actor'
      Source
        .queue[WsNotification[_]](maxBufferSize, OverflowStrategy.dropHead)
        .toMat(BroadcastHub.sink)(Keep.both)
        .run()
    }

    //need to drain the websocket queue if no one is connected
    val _: Future[Done] = tuple._2.runWith(Sink.ignore)

    tuple
  }

  private def handleDuplicateSpendingInfoDb(
      wallet: AnyDLCHDWalletApi,
      walletConfig: WalletAppConfig): Future[Unit] = {
    val spendingInfoDAO = SpendingInfoDAO()(ec, walletConfig)
    for {
      rescanNeeded <- spendingInfoDAO.hasDuplicates()
      _ <-
        if (rescanNeeded) {
          logger.warn("Found duplicate UTXOs. Rescanning...")
          wallet
            .rescanNeutrinoWallet(startOpt = None,
                                  endOpt = None,
                                  addressBatchSize =
                                    wallet.discoveryBatchSize(),
                                  useCreationTime = true,
                                  force = true)
            .recover { case scala.util.control.NonFatal(exn) =>
              logger.error(s"Failed to handleDuplicateSpendingInfoDb rescan",
                           exn)
              RescanState.RescanDone
            }
        } else {
          Future.successful(RescanState.RescanDone)
        }
      _ <- spendingInfoDAO.createOutPointsIndexIfNeeded()
    } yield ()
  }

  private def restartRescanIfNeeded(
      wallet: AnyDLCHDWalletApi): Future[RescanState] = {
    for {
      isRescanning <- wallet.isRescanning()
      res <-
        if (isRescanning)
          wallet.rescanNeutrinoWallet(startOpt = None,
                                      endOpt = None,
                                      addressBatchSize =
                                        wallet.discoveryBatchSize(),
                                      useCreationTime = true,
                                      force = true)
        else Future.successful(RescanState.RescanDone)
    } yield res
  }

  private lazy val nodeStateDAO: NodeStateDescriptorDAO =
    NodeStateDescriptorDAO()(ec, nodeConf)

  private def getLastLoadedWalletName(): Future[Option[String]] = {
    nodeStateDAO
      .getWalletName()
      .recover { case _: SQLException => None }
      .map(_.map(_.walletName))
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
    BitcoinSAppConfig(
      datadirParser.datadir,
      Vector(datadirParser.baseConfig, serverCmdLineArgs.toConfig))(system)

  val m = new BitcoinSServerMain(serverCmdLineArgs)

  m.run()

  sys.addShutdownHook {
    logger.info(
      s"@@@@@@@@@@@@@@@@@@@@@ Shutting down ${getClass.getSimpleName} @@@@@@@@@@@@@@@@@@@@@")
    Await.result(m.stop(), 10.seconds)
  }
}
