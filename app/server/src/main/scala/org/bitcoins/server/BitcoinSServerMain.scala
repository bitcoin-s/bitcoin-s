package org.bitcoins.server

import akka.{Done, NotUsed}
import akka.actor.ActorSystem
import akka.dispatch.Dispatchers
import akka.http.scaladsl.model.ws.Message
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{
  BroadcastHub,
  Keep,
  Sink,
  Source,
  SourceQueueWithComplete
}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models._
import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockChainInfoResult
import org.bitcoins.commons.util.{DatadirParser, ServerArgParser}
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.{
  InternalImplementationNodeType,
  NodeApi,
  NodeType
}
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.dlc.node.DLCNode
import org.bitcoins.dlc.node.config.DLCNodeAppConfig
import org.bitcoins.dlc.wallet._
import org.bitcoins.feeprovider.FeeProviderName._
import org.bitcoins.feeprovider.MempoolSpaceTarget.HourFeeTarget
import org.bitcoins.feeprovider._
import org.bitcoins.node._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.rpc.BitcoindException.InWarmUp
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.{BitcoindRpcAppConfig, ZmqConfig}
import org.bitcoins.server.routes.{BitcoinSServerRunner, CommonRoutes, Server}
import org.bitcoins.server.util.{
  BitcoinSAppScalaDaemon,
  CallbackUtil,
  ServerBindings,
  WebsocketUtil,
  WsServerConfig
}
import org.bitcoins.tor.config.TorAppConfig
import org.bitcoins.wallet._
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future, Promise}

class BitcoinSServerMain(override val serverArgParser: ServerArgParser)(implicit
    override val system: ActorSystem,
    val conf: BitcoinSAppConfig)
    extends BitcoinSServerRunner {

  implicit lazy val walletConf: WalletAppConfig = conf.walletConf
  implicit lazy val nodeConf: NodeAppConfig = conf.nodeConf
  implicit lazy val chainConf: ChainAppConfig = conf.chainConf
  implicit lazy val dlcConf: DLCAppConfig = conf.dlcConf
  implicit lazy val dlcNodeConf: DLCNodeAppConfig = conf.dlcNodeConf
  implicit lazy val bitcoindRpcConf: BitcoindRpcAppConfig = conf.bitcoindRpcConf
  implicit lazy val torConf: TorAppConfig = conf.torConf

  override def start(): Future[Unit] = {
    logger.info("Starting appServer")
    val startTime = TimeUtil.currentEpochMs
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

  def startBitcoinSBackend(): Future[Unit] = {
    logger.info(s"startBitcoinSBackend()")
    val start = System.currentTimeMillis()

    //run chain work migration
    val chainApiF = runChainWorkCalc(
      serverArgParser.forceChainWorkRecalc || chainConf.forceRecalcChainWork)

    //get a node that isn't started
    val nodeF = nodeConf.createNode()(chainConf, system)

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
      nodeCallbacks <- CallbackUtil.createNeutrinoNodeCallbacksForWallet(wallet)
      _ = nodeConf.addCallbacks(nodeCallbacks)
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

    val tuple = buildWsSource

    val wsQueue: SourceQueueWithComplete[Message] = tuple._1
    val wsSource: Source[Message, NotUsed] = tuple._2
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

      _ <- startHttpServer(nodeApi = node,
                           chainApi = chainApi,
                           wallet = wallet,
                           dlcNode = dlcNode,
                           serverCmdLineArgs = serverArgParser,
                           wsSource = wsSource)
      chainCallbacks = WebsocketUtil.buildChainCallbacks(wsQueue, chainApi)
      _ = chainConf.addCallbacks(chainCallbacks)
      walletCallbacks = WebsocketUtil.buildWalletCallbacks(wsQueue)
      _ = walletConf.addCallbacks(walletCallbacks)
      dlcWalletCallbacks = WebsocketUtil.buildDLCWalletCallbacks(wsQueue)
      _ = dlcConf.addCallbacks(dlcWalletCallbacks)
      _ = {
        logger.info(
          s"Starting ${nodeConf.nodeType.shortName} node sync, it took=${System
            .currentTimeMillis() - start}ms")
      }
      _ <- node.sync()
    } yield {
      logger.info(
        s"Done starting Main! It took ${System.currentTimeMillis() - start}ms")
      ()
    }
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
        interval = 1.second,
        maxTries = 100
      )
      info <- promise.future
    } yield info
  }

  def startBitcoindBackend(): Future[Unit] = {
    logger.info(s"startBitcoindBackend()")
    val bitcoindF = for {
      client <- bitcoindRpcConf.clientF
      _ <- client.start()
    } yield client

    val tmpWalletF = bitcoindF.flatMap { bitcoind =>
      val feeProvider = getFeeProviderOrElse(bitcoind)
      dlcConf.createDLCWallet(nodeApi = bitcoind,
                              chainQueryApi = bitcoind,
                              feeRateApi = feeProvider)
    }

    val tuple = buildWsSource
    val wsQueue: SourceQueueWithComplete[Message] = tuple._1
    val wsSource: Source[Message, NotUsed] = tuple._2

    for {
      _ <- bitcoindRpcConf.start()
      bitcoind <- bitcoindRpcConf.clientF
      _ = logger.info("Started bitcoind")

      bitcoindNetwork <- getBlockChainInfo(bitcoind).map(_.chain)
      _ = require(
        bitcoindNetwork == walletConf.network,
        s"bitcoind ($bitcoindNetwork) on different network than wallet (${walletConf.network})")

      _ = logger.info("Creating wallet")
      chainCallbacks = WebsocketUtil.buildChainCallbacks(wsQueue, bitcoind)
      tmpWallet <- tmpWalletF
      wallet = BitcoindRpcBackendUtil.createDLCWalletWithBitcoindCallbacks(
        bitcoind,
        tmpWallet,
        Some(chainCallbacks))
      _ = logger.info("Starting wallet")
      _ <- wallet.start().recoverWith {
        //https://github.com/bitcoin-s/bitcoin-s/issues/2917
        //https://github.com/bitcoin-s/bitcoin-s/pull/2918
        case err: IllegalArgumentException
            if err.getMessage.contains("If we have spent a spendinginfodb") =>
          handleMissingSpendingInfoDb(err, wallet)
      }

      dlcNode = dlcNodeConf.createDLCNode(wallet)
      _ <- dlcNode.start()
      _ <- startHttpServer(nodeApi = bitcoind,
                           chainApi = bitcoind,
                           wallet = wallet,
                           dlcNode = dlcNode,
                           serverCmdLineArgs = serverArgParser,
                           wsSource = wsSource)
      walletCallbacks = WebsocketUtil.buildWalletCallbacks(wsQueue)
      _ = walletConf.addCallbacks(walletCallbacks)

      //intentionally doesn't map on this otherwise we
      //wait until we are done syncing the entire wallet
      //which could take 1 hour
      _ = syncWalletWithBitcoindAndStartPolling(bitcoind, wallet)
      dlcWalletCallbacks = WebsocketUtil.buildDLCWalletCallbacks(wsQueue)
      _ = dlcConf.addCallbacks(dlcWalletCallbacks)
    } yield {
      logger.info(s"Done starting Main!")
      ()
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

  private var serverBindingsOpt: Option[ServerBindings] = None

  private def startHttpServer(
      nodeApi: NodeApi,
      chainApi: ChainApi,
      wallet: DLCWallet,
      dlcNode: DLCNode,
      serverCmdLineArgs: ServerArgParser,
      wsSource: Source[Message, NotUsed])(implicit
      system: ActorSystem,
      conf: BitcoinSAppConfig): Future[Server] = {
    implicit val nodeConf: NodeAppConfig = conf.nodeConf
    implicit val walletConf: WalletAppConfig = conf.walletConf

    val walletRoutes = WalletRoutes(wallet)
    val nodeRoutes = NodeRoutes(nodeApi)
    val chainRoutes = ChainRoutes(chainApi, nodeConf.network)
    val coreRoutes = CoreRoutes()
    val dlcRoutes = DLCRoutes(dlcNode)
    val commonRoutes = CommonRoutes(conf.baseDatadir)

    val handlers =
      Seq(walletRoutes,
          nodeRoutes,
          chainRoutes,
          coreRoutes,
          dlcRoutes,
          commonRoutes)

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
                 handlers = handlers,
                 rpcbindOpt = rpcBindConfOpt,
                 rpcport = rpcport,
                 rpcPassword = conf.rpcPassword,
                 wsConfigOpt = Some(wsServerConfig),
                 wsSource)
        case None =>
          Server(
            conf = nodeConf,
            handlers = handlers,
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
      wallet: Wallet): Future[Unit] = {
    val f = BitcoindRpcBackendUtil
      .syncWalletToBitcoind(bitcoind, wallet)
      .flatMap(_ => wallet.updateUtxoPendingStates())
      .flatMap { _ =>
        if (bitcoindRpcConf.zmqConfig == ZmqConfig.empty) {
          BitcoindRpcBackendUtil
            .startBitcoindBlockPolling(wallet, bitcoind)
            .map(_ => ())
        } else {
          Future {
            BitcoindRpcBackendUtil.startZMQWalletCallbacks(
              wallet,
              bitcoindRpcConf.zmqConfig)
          }
        }
      }

    f.failed.foreach(err =>
      logger.error(s"Error syncing bitcoin-s wallet with bitcoind", err))
    f
  }

  /** Builds a websocket queue that you can feed elements to.
    * The Source can be wired up with Directives.handleWebSocketMessages
    * to create a flow that emits websocket messages
    */
  private def buildWsSource: (
      SourceQueueWithComplete[Message],
      Source[Message, NotUsed]) = {
    val maxBufferSize: Int = 25

    /** This will queue [[maxBufferSize]] elements in the queue. Once the buffer size is reached,
      * we will drop the first element in the buffer
      */
    val tuple = {
      //from: https://github.com/akka/akka-http/issues/3039#issuecomment-610263181
      //the BroadcastHub.sink is needed to avoid these errors
      // 'Websocket handler failed with Processor actor'
      Source
        .queue[Message](maxBufferSize, OverflowStrategy.dropHead)
        .toMat(BroadcastHub.sink)(Keep.both)
        .run()
    }

    //need to drain the websocket queue if no one is connected
    val _: Future[Done] = tuple._2.runWith(Sink.ignore)

    tuple
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

  new BitcoinSServerMain(serverCmdLineArgs).run()
}
