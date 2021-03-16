package org.bitcoins.testkit.node

import java.net.InetSocketAddress
import akka.actor.{ActorSystem, Cancellable}
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.chain.{ChainApi, ChainQueryApi, FilterSyncMarker}
import org.bitcoins.core.api.chain.db.{
  BlockHeaderDb,
  CompactFilterDb,
  CompactFilterHeaderDb
}
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.gcs.FilterHeader
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.AppConfig
import org.bitcoins.node._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.{
  PeerHandler,
  PeerMessageReceiver,
  PeerMessageReceiverState,
  PeerMessageSender
}
import org.bitcoins.rpc.client.common.BitcoindVersion.{V18, V19}
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.BitcoinSAppConfig._
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.chain.ChainUnitTest
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.keymanager.KeyManagerTestUtil
import org.bitcoins.testkit.node.NodeUnitTest.{
  createPeer,
  emptyPeer,
  syncNeutrinoNode
}
import org.bitcoins.testkit.node.fixture.{
  NeutrinoNodeConnectedWithBitcoind,
  NodeConnectedWithBitcoind,
  SpvNodeConnectedWithBitcoind,
  SpvNodeConnectedWithBitcoindV19
}
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletWithBitcoindRpc}
import org.bitcoins.testkitcore.node.P2PMessageTestUtil
import org.bitcoins.wallet.WalletCallbacks
import org.scalatest.FutureOutcome

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

trait NodeUnitTest extends BitcoinSFixture with EmbeddedPg with BitcoinSLogger {

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(getFreshConfig.nodeConf)
    super[EmbeddedPg].beforeAll()
  }

  override def afterAll(): Unit = {
    super[EmbeddedPg].afterAll()
  }

  /** Wallet config with data directory set to user temp directory */
  implicit protected def getFreshConfig: BitcoinSAppConfig

  implicit override lazy val np: NetworkParameters =
    getFreshConfig.nodeConf.network

  lazy val startedBitcoindF = BitcoindRpcTestUtil.startedBitcoindRpcClient()

  lazy val bitcoindPeerF = startedBitcoindF.map(NodeTestUtil.getBitcoindPeer)

  lazy val junkAddress: BitcoinAddress =
    BitcoinAddress("2NFyxovf6MyxfHqtVjstGzs6HeLqv92Nq4U")

  val genesisChainApi: ChainApi = new ChainApi {

    override def processHeaders(
        headers: Vector[BlockHeader]): Future[ChainApi] =
      Future.successful(this)

    override def getHeader(
        hash: DoubleSha256DigestBE): Future[Option[BlockHeaderDb]] =
      Future.successful(None)

    override def getHeadersAtHeight(
        height: Int): Future[Vector[BlockHeaderDb]] =
      Future.successful(Vector.empty)

    override def getBlockCount(): Future[Int] = Future.successful(0)

    override def getBestBlockHeader(): Future[BlockHeaderDb] =
      Future.successful(ChainUnitTest.genesisHeaderDb)

    override def processFilterHeaders(
        filterHeaders: Vector[FilterHeader],
        stopHash: DoubleSha256DigestBE): Future[ChainApi] =
      Future.successful(this)

    override def nextBlockHeaderBatchRange(
        stopHash: DoubleSha256DigestBE,
        batchSize: Int): Future[Option[FilterSyncMarker]] =
      Future.successful(None)

    override def nextFilterHeaderBatchRange(
        startHeight: Int,
        batchSize: Int): Future[Option[FilterSyncMarker]] =
      Future.successful(None)

    override def processFilters(
        message: Vector[CompactFilterMessage]): Future[ChainApi] =
      Future.successful(this)

    override def processCheckpoints(
        checkpoints: Vector[DoubleSha256DigestBE],
        blockHash: DoubleSha256DigestBE): Future[ChainApi] =
      Future.successful(this)

    override def getFilterHeaderCount(): Future[Int] = Future.successful(0)

    override def getFilterHeadersAtHeight(
        height: Int): Future[Vector[CompactFilterHeaderDb]] =
      Future.successful(Vector.empty)

    override def getBestFilterHeader(): Future[Option[CompactFilterHeaderDb]] =
      Future.successful(None)

    override def getFilterHeader(blockHash: DoubleSha256DigestBE): Future[
      Option[CompactFilterHeaderDb]] = Future.successful(None)

    override def getFilter(
        hash: DoubleSha256DigestBE): Future[Option[CompactFilterDb]] =
      Future.successful(None)

    override def getFilterCount(): Future[Int] = Future.successful(0)

    override def getFiltersAtHeight(
        height: Int): Future[Vector[CompactFilterDb]] =
      Future.successful(Vector.empty)

    override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
      Future.successful(0)

    override def getHeadersBetween(
        from: BlockHeaderDb,
        to: BlockHeaderDb): Future[Vector[BlockHeaderDb]] =
      Future.successful(Vector.empty)

    override def getBlockHeight(
        blockHash: DoubleSha256DigestBE): Future[Option[Int]] =
      Future.successful(None)

    override def getBestBlockHash(): Future[DoubleSha256DigestBE] =
      Future.successful(DoubleSha256DigestBE.empty)

    override def getNumberOfConfirmations(
        blockHashOpt: DoubleSha256DigestBE): Future[Option[Int]] =
      Future.successful(None)

    override def getFiltersBetweenHeights(
        startHeight: Int,
        endHeight: Int): Future[Vector[ChainQueryApi.FilterResponse]] =
      Future.successful(Vector.empty)

    override def epochSecondToBlockHeight(time: Long): Future[Int] =
      Future.successful(0)
  }

  def withDisconnectedSpvNode(test: OneArgAsyncTest)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {

    val nodeBuilder: () => Future[SpvNode] = { () =>
      require(appConfig.nodeType == NodeType.SpvNode)
      for {
        node <- NodeUnitTest.createSpvNode(emptyPeer)(system,
                                                      appConfig.chainConf,
                                                      appConfig.nodeConf)
        //we aren't calling node.start(), but we need to call appConfig.start()
        //to make sure migrations are run
        _ <- node.chainConfig.start()
        _ <- node.nodeConfig.start()
      } yield node
    }

    makeDependentFixture(
      build = nodeBuilder,
      destroy = (_: Node) => {
        for {
          _ <- ChainUnitTest.destroyAllTables()(appConfig.chainConf,
                                                system.dispatcher)
          _ <- appConfig.stop()
        } yield ()
      }
    )(test)
  }

  def withSpvNodeConnectedToBitcoind(
      test: OneArgAsyncTest,
      versionOpt: Option[BitcoindVersion] = None)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    val nodeWithBitcoindBuilder: () => Future[SpvNodeConnectedWithBitcoind] = {
      () =>
        require(appConfig.nodeType == NodeType.SpvNode)
        for {
          bitcoind <- BitcoinSFixture.createBitcoind(versionOpt)
          node <- NodeUnitTest.createSpvNode(createPeer(bitcoind))(
            system,
            appConfig.chainConf,
            appConfig.nodeConf)
          started <- node.start()
          _ <- NodeUnitTest.syncSpvNode(started, bitcoind)
        } yield SpvNodeConnectedWithBitcoind(node, bitcoind)
    }

    makeDependentFixture(
      build = nodeWithBitcoindBuilder,
      destroy = NodeUnitTest.destroyNodeConnectedWithBitcoind(
        _: NodeConnectedWithBitcoind)(system, appConfig)
    )(test)
  }

  def withSpvNodeConnectedToBitcoindV19(test: OneArgAsyncTest)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    val nodeWithBitcoindBuilder: () => Future[
      SpvNodeConnectedWithBitcoindV19] = { () =>
      require(appConfig.nodeType == NodeType.SpvNode)
      for {
        bitcoind <-
          BitcoinSFixture
            .createBitcoindWithFunds(Some(V19))
            .map(_.asInstanceOf[BitcoindV19RpcClient])
        node <- NodeUnitTest.createSpvNode(createPeer(bitcoind))(
          system,
          appConfig.chainConf,
          appConfig.nodeConf)
        started <- node.start()
        _ <- NodeUnitTest.syncSpvNode(started, bitcoind)
      } yield SpvNodeConnectedWithBitcoindV19(node, bitcoind)
    }

    makeDependentFixture(
      build = nodeWithBitcoindBuilder,
      destroy = NodeUnitTest.destroyNodeConnectedWithBitcoind(
        _: NodeConnectedWithBitcoind)(system, appConfig)
    )(test)
  }

  def withNeutrinoNodeConnectedToBitcoind(
      test: OneArgAsyncTest,
      versionOpt: Option[BitcoindVersion] = None)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    val nodeWithBitcoindBuilder: () => Future[
      NeutrinoNodeConnectedWithBitcoind] = { () =>
      require(appConfig.nodeType == NodeType.NeutrinoNode)
      for {
        bitcoind <- BitcoinSFixture.createBitcoind(versionOpt)
        node <- NodeUnitTest.createNeutrinoNode(bitcoind)(system,
                                                          appConfig.chainConf,
                                                          appConfig.nodeConf)
        startedNode <- node.start()
        syncedNode <- syncNeutrinoNode(startedNode, bitcoind)
      } yield NeutrinoNodeConnectedWithBitcoind(syncedNode, bitcoind)
    }
    makeDependentFixture(
      build = nodeWithBitcoindBuilder,
      destroy = NodeUnitTest.destroyNodeConnectedWithBitcoind(
        _: NodeConnectedWithBitcoind)(system, appConfig)
    )(test)
  }

  def withSpvNodeFundedWalletBitcoind(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String])(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {

    makeDependentFixture(
      build = () =>
        NodeUnitTest.createSpvNodeFundedWalletBitcoind(bip39PasswordOpt =
                                                         bip39PasswordOpt,
                                                       versionOpt = Option(V18),
                                                       walletCallbacks =
                                                         WalletCallbacks.empty)(
          system, // Force V18 because Spv is disabled on versions after
          appConfig),
      destroy = NodeUnitTest.destroyNodeFundedWalletBitcoind(
        _: NodeFundedWalletBitcoind)(system, appConfig)
    )(test)
  }

  def withNeutrinoNodeFundedWalletBitcoind(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String],
      versionOpt: Option[BitcoindVersion] = None,
      walletCallbacks: WalletCallbacks = WalletCallbacks.empty)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    makeDependentFixture(
      build = () =>
        NodeUnitTest
          .createNeutrinoNodeFundedWalletBitcoind(
            bip39PasswordOpt = bip39PasswordOpt,
            versionOpt = versionOpt,
            walletCallbacks = walletCallbacks)(system, appConfig),
      destroy = NodeUnitTest.destroyNodeFundedWalletBitcoind(
        _: NodeFundedWalletBitcoind)(system, appConfig)
    )(test)
  }

  /** Helper method to generate blocks every interval
    * @return a cancellable that will stop generating blocks
    */
  def genBlockInterval(bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem): Cancellable = {

    var counter = 0
    val desiredBlocks = 5
    val interval = 500.millis

    val genBlock = new Runnable {
      override def run(): Unit = {
        if (counter < desiredBlocks) {
          bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(1, _))
          counter = counter + 1
        }
      }
    }

    system.scheduler.scheduleAtFixedRate(2.second, interval)(genBlock)
  }

  def getBIP39PasswordOpt(): Option[String] =
    KeyManagerTestUtil.bip39PasswordOpt
}

object NodeUnitTest extends P2PLogger {

  def buildPeerMessageReceiver(chainApi: ChainApi, peer: Peer)(implicit
      appConfig: BitcoinSAppConfig,
      system: ActorSystem): Future[PeerMessageReceiver] = {
    val receiver =
      PeerMessageReceiver(state = PeerMessageReceiverState.fresh(),
                          chainApi = chainApi,
                          peer = peer,
                          initialSyncDone = None)
    Future.successful(receiver)
  }

  def buildPeerHandler(peer: Peer)(implicit
      nodeAppConfig: NodeAppConfig,
      chainAppConfig: ChainAppConfig,
      system: ActorSystem): Future[PeerHandler] = {
    import system.dispatcher
    val chainApiF = ChainUnitTest.createChainHandler()
    val peerMsgReceiverF = chainApiF.flatMap { _ =>
      PeerMessageReceiver.preConnection(peer, None)
    }
    //the problem here is the 'self', this needs to be an ordinary peer message handler
    //that can handle the handshake
    val peerHandlerF = for {
      peerMsgReceiver <- peerMsgReceiverF
      client = NodeTestUtil.client(peer, peerMsgReceiver)
      peerMsgSender = PeerMessageSender(client)
    } yield PeerHandler(client, peerMsgSender)

    peerHandlerF

  }

  def destroyNode(node: Node)(implicit ec: ExecutionContext): Future[Unit] = {
    for {
      _ <- node.stop()
    } yield ()
  }

  def destroyNodeConnectedWithBitcoind(
      nodeConnectedWithBitcoind: NodeConnectedWithBitcoind)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[Unit] = {
    logger.debug(s"Beginning tear down of node connected with bitcoind")
    import system.dispatcher
    val node = nodeConnectedWithBitcoind.node
    val bitcoind = nodeConnectedWithBitcoind.bitcoind
    val resultF = for {
      _ <- destroyNode(node)
      _ <- ChainUnitTest.destroyBitcoind(bitcoind)
      _ = cleanTables(appConfig)
      _ <- appConfig.stop()
    } yield {
      logger.debug(s"Done with teardown of node connected with bitcoind!")
      ()
    }

    resultF
  }

  /** Creates a spv node, a funded bitcoin-s wallet, all of which are connected to bitcoind */
  def createSpvNodeFundedWalletBitcoind(
      walletCallbacks: WalletCallbacks,
      bip39PasswordOpt: Option[String],
      versionOpt: Option[BitcoindVersion] = None)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[SpvNodeFundedWalletBitcoind] = {
    import system.dispatcher
    require(appConfig.nodeType == NodeType.SpvNode)
    for {
      bitcoind <- BitcoinSFixture.createBitcoindWithFunds(versionOpt)
      node <- createSpvNode(createPeer(bitcoind))
      fundedWallet <- BitcoinSWalletTest.fundedWalletAndBitcoind(
        bitcoind,
        node,
        node,
        bip39PasswordOpt,
        walletCallbacks)
      spvCallbacks =
        BitcoinSWalletTest.createSpvNodeCallbacksForWallet(fundedWallet.wallet)
      _ = appConfig.nodeConf.addCallbacks(spvCallbacks)
      walletBloomFilter <- fundedWallet.wallet.getBloomFilter()
      withBloomFilter = node.setBloomFilter(walletBloomFilter)
      startedNodeWithBloomFilter <- withBloomFilter.start()
      _ <- syncSpvNode(startedNodeWithBloomFilter, bitcoind)
      //callbacks are executed asynchronously, which is how we fund the wallet
      //so we need to wait until the wallet balances are correct
      _ <- BitcoinSWalletTest.awaitWalletBalances(fundedWallet)
    } yield {
      SpvNodeFundedWalletBitcoind(node = startedNodeWithBloomFilter,
                                  wallet = fundedWallet.wallet,
                                  bitcoindRpc = fundedWallet.bitcoind,
                                  bip39PasswordOpt)
    }
  }

  /** Creates a neutrino node, a funded bitcoin-s wallet, all of which are connected to bitcoind */
  def createNeutrinoNodeFundedWalletBitcoind(
      bip39PasswordOpt: Option[String],
      versionOpt: Option[BitcoindVersion],
      walletCallbacks: WalletCallbacks)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[
    NeutrinoNodeFundedWalletBitcoind] = {
    import system.dispatcher
    require(appConfig.nodeType == NodeType.NeutrinoNode)
    for {
      bitcoind <- BitcoinSFixture.createBitcoindWithFunds(versionOpt)
      node <- createNeutrinoNode(bitcoind)
      fundedWallet <- BitcoinSWalletTest.fundedWalletAndBitcoind(
        bitcoindRpcClient = bitcoind,
        nodeApi = node,
        chainQueryApi = node,
        bip39PasswordOpt = bip39PasswordOpt,
        walletCallbacks = walletCallbacks)
      startedNode <- node.start()
      syncedNode <- syncNeutrinoNode(startedNode, bitcoind)
      //callbacks are executed asynchronously, which is how we fund the wallet
      //so we need to wait until the wallet balances are correct
      _ <- BitcoinSWalletTest.awaitWalletBalances(fundedWallet)
    } yield {
      NeutrinoNodeFundedWalletBitcoind(node = syncedNode,
                                       wallet = fundedWallet.wallet,
                                       bitcoindRpc = fundedWallet.bitcoind,
                                       bip39PasswordOpt = bip39PasswordOpt)
    }
  }

  def destroyNodeFundedWalletBitcoind(
      fundedWalletBitcoind: NodeFundedWalletBitcoind)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[Unit] = {
    import system.dispatcher
    val walletWithBitcoind = {
      WalletWithBitcoindRpc(fundedWalletBitcoind.wallet,
                            fundedWalletBitcoind.bitcoindRpc)
    }

    //these need to be done in order, as the spv node needs to be
    //stopped before the bitcoind node is stopped
    val destroyedF = for {
      _ <- BitcoinSWalletTest.destroyWallet(walletWithBitcoind.wallet)
      _ <- destroyNodeConnectedWithBitcoind(
        fundedWalletBitcoind.toNodeConnectedWithBitcoind)
    } yield ()

    destroyedF
  }

  def buildPeerMessageReceiver(chainApi: ChainApi, peer: Peer)(implicit
      nodeAppConfig: NodeAppConfig,
      chainAppConfig: ChainAppConfig,
      system: ActorSystem): Future[PeerMessageReceiver] = {
    val receiver =
      PeerMessageReceiver(state = PeerMessageReceiverState.fresh(),
                          chainApi = chainApi,
                          peer = peer,
                          initialSyncDone = None)
    Future.successful(receiver)
  }

  def peerSocketAddress(
      bitcoindRpcClient: BitcoindRpcClient): InetSocketAddress = {
    NodeTestUtil.getBitcoindSocketAddress(bitcoindRpcClient)
  }

  def createPeer(bitcoind: BitcoindRpcClient): Peer = {
    val socket = peerSocketAddress(bitcoind)
    Peer(id = None, socket = socket)
  }

  def emptyPeer: Peer = {
    val socket = new InetSocketAddress(RpcUtil.randomPort)
    Peer(id = None, socket = socket)
  }

  /** Creates a spv node peered with the given bitcoind client
    * This does NOT start the spv node
    */
  def createSpvNode(peer: Peer)(implicit
      system: ActorSystem,
      chainAppConfig: ChainAppConfig,
      nodeAppConfig: NodeAppConfig): Future[SpvNode] = {
    import system.dispatcher

    val checkConfigF = Future {
      assert(nodeAppConfig.nodeType == NodeType.SpvNode)
    }
    val chainApiF = for {
      _ <- checkConfigF
      chainHandler <- ChainUnitTest.createChainHandler()
    } yield chainHandler
    val nodeF = for {
      _ <- chainApiF
    } yield {
      SpvNode(
        nodePeer = peer,
        nodeConfig = nodeAppConfig,
        chainConfig = chainAppConfig,
        actorSystem = system,
        initialSyncDone = None
      ).setBloomFilter(P2PMessageTestUtil.emptyBloomFilter)
    }

    nodeF

  }

  /** Creates a Neutrino node peered with the given bitcoind client, this method
    * also calls [[org.bitcoins.node.Node.start() start]] to start the node
    */
  def createNeutrinoNode(bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem,
      chainAppConfig: ChainAppConfig,
      nodeAppConfig: NodeAppConfig): Future[NeutrinoNode] = {
    import system.dispatcher

    val checkConfigF = Future {
      assert(nodeAppConfig.nodeType == NodeType.NeutrinoNode)
    }
    val chainApiF = for {
      _ <- checkConfigF
      chainHandler <- ChainUnitTest.createChainHandler()
    } yield chainHandler
    val peer = createPeer(bitcoind)
    val nodeF = for {
      _ <- chainApiF
    } yield {
      NeutrinoNode(nodePeer = peer,
                   nodeConfig = nodeAppConfig,
                   chainConfig = chainAppConfig,
                   actorSystem = system,
                   initialSyncDone = None)
    }

    nodeF
  }

  def syncNeutrinoNode(node: NeutrinoNode, bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem): Future[NeutrinoNode] = {
    import system.dispatcher
    for {
      _ <- node.sync()
      _ <- NodeTestUtil.awaitSync(node, bitcoind)
      _ <- NodeTestUtil.awaitCompactFilterHeadersSync(node, bitcoind)
      _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)
    } yield node
  }

  def syncSpvNode(node: SpvNode, bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem): Future[SpvNode] = {
    import system.dispatcher
    for {
      _ <- node.sync()
      _ <- NodeTestUtil.awaitSync(node, bitcoind)
    } yield node
  }

  /** This is needed for postgres, we do not drop tables in between individual tests with postgres
    * rather an entire test suite shares the same postgres database.
    * therefore, we need to clean the database after each test, so that migrations can be applied during
    * the setup phase for the next test.
    * @param appConfig
    */
  private def cleanTables(appConfig: BitcoinSAppConfig): Unit = {
    appConfig.nodeConf.clean()
    //appConfig.walletConf.clean()
    appConfig.chainConf.clean()
  }
}
