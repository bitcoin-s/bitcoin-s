package org.bitcoins.testkit.node

import akka.actor.ActorSystem
import org.bitcoins.chain.blockchain.ChainHandlerCached
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models._
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.node.NodeType
import org.bitcoins.node._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer._
import org.bitcoins.rpc.client.common.BitcoindVersion.{V18, V21, V22}
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.client.v21.BitcoindV21RpcClient
import org.bitcoins.rpc.client.v22.BitcoindV22RpcClient
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.chain.ChainUnitTest
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.node.NodeUnitTest.{
  createPeer,
  emptyPeer,
  syncNeutrinoNode
}
import org.bitcoins.testkit.node.fixture._
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletWithBitcoindRpc}
import org.bitcoins.testkitcore.node.P2PMessageTestUtil
import org.bitcoins.wallet.WalletCallbacks
import org.scalatest.FutureOutcome

import java.net.InetSocketAddress
import scala.concurrent.{ExecutionContext, Future}

trait NodeUnitTest extends BaseNodeTest {

  def withDisconnectedSpvNode(test: OneArgAsyncTest)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {

    val nodeBuilder: () => Future[SpvNode] = { () =>
      require(appConfig.nodeConf.nodeType == NodeType.SpvNode)
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
        require(appConfig.nodeConf.nodeType == NodeType.SpvNode)
        for {
          bitcoind <- BitcoinSFixture.createBitcoind(versionOpt)
          peer <- createPeer(bitcoind)
          node <- NodeUnitTest.createSpvNode(peer)(system,
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

  def withSpvNodeConnectedToBitcoindV21(test: OneArgAsyncTest)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    val nodeWithBitcoindBuilder: () => Future[
      SpvNodeConnectedWithBitcoindV21] = { () =>
      require(appConfig.nodeConf.nodeType == NodeType.SpvNode)
      for {
        bitcoind <-
          BitcoinSFixture
            .createBitcoindWithFunds(Some(V21))
            .map(_.asInstanceOf[BitcoindV21RpcClient])
        peer <- createPeer(bitcoind)
        node <- NodeUnitTest.createSpvNode(peer)(system,
                                                 appConfig.chainConf,
                                                 appConfig.nodeConf)
        started <- node.start()
        _ <- NodeUnitTest.syncSpvNode(started, bitcoind)
      } yield SpvNodeConnectedWithBitcoindV21(node, bitcoind)
    }

    makeDependentFixture(
      build = nodeWithBitcoindBuilder,
      destroy = NodeUnitTest.destroyNodeConnectedWithBitcoind(
        _: NodeConnectedWithBitcoind)(system, appConfig)
    )(test)
  }

  def withSpvNodeConnectedToBitcoindV22(test: OneArgAsyncTest)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    val nodeWithBitcoindBuilder: () => Future[
      SpvNodeConnectedWithBitcoindV22] = { () =>
      require(appConfig.nodeConf.nodeType == NodeType.SpvNode)
      for {
        bitcoind <-
          BitcoinSFixture
            .createBitcoindWithFunds(Some(V22))
            .map(_.asInstanceOf[BitcoindV22RpcClient])
        peer <- createPeer(bitcoind)
        node <- NodeUnitTest.createSpvNode(peer)(system,
                                                 appConfig.chainConf,
                                                 appConfig.nodeConf)
        started <- node.start()
        _ <- NodeUnitTest.syncSpvNode(started, bitcoind)
      } yield SpvNodeConnectedWithBitcoindV22(node, bitcoind)
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
      require(appConfig.nodeConf.nodeType == NodeType.NeutrinoNode)
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
}

object NodeUnitTest extends P2PLogger {

  def buildNode(peer: Peer)(implicit
      chainConf: ChainAppConfig,
      nodeConf: NodeAppConfig,
      system: ActorSystem): Future[NeutrinoNode] = {
    import system.dispatcher

    val blockHeaderDAO = BlockHeaderDAO()
    val filterHeaderDAO = CompactFilterHeaderDAO()
    val filterDAO = CompactFilterDAO()

    val chainApiF = ChainHandlerCached
      .fromDatabase(blockHeaderDAO, filterHeaderDAO, filterDAO)

    val nodeF = chainApiF.map(buildNode(peer, _))
    for {
      node <- nodeF
      _ <- node.nodeConfig.start()
      peers <- node.peerManager.getPeers
    } yield {
      peers.foreach(node.peerManager.addPeer)
      node
    }
  }

  def buildNode(peer: Peer, chainApi: ChainApi)(implicit
      chainConf: ChainAppConfig,
      nodeConf: NodeAppConfig,
      system: ActorSystem): NeutrinoNode = {
    import system.dispatcher

    val dmh = DataMessageHandler(chainApi)

    NeutrinoNode(dmh,
                 nodeConf,
                 chainConf,
                 system,
                 configPeersOverride = Vector(peer))
  }

  def buildPeerMessageReceiver(chainApi: ChainApi, peer: Peer)(implicit
      appConfig: BitcoinSAppConfig,
      system: ActorSystem): Future[PeerMessageReceiver] = {
    val receiver =
      PeerMessageReceiver(state = PeerMessageReceiverState.fresh(),
                          node = buildNode(peer, chainApi)(appConfig.chainConf,
                                                           appConfig.nodeConf,
                                                           system),
                          peer = peer)(system, appConfig.nodeConf)
    Future.successful(receiver)
  }

  def buildPeerHandler(peer: Peer)(implicit
      nodeAppConfig: NodeAppConfig,
      chainAppConfig: ChainAppConfig,
      system: ActorSystem): Future[PeerHandler] = {
    import system.dispatcher
    val nodeF = buildNode(peer)
    val peerMsgReceiverF = nodeF.map { node =>
      PeerMessageReceiver.preConnection(peer, node)
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
      _ <- node.nodeAppConfig.stop()
      _ <- node.chainAppConfig.stop()
    } yield {
      ()
    }
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
    require(appConfig.nodeConf.nodeType == NodeType.SpvNode)
    for {
      bitcoind <- BitcoinSFixture.createBitcoindWithFunds(versionOpt)
      spvNodeWithBitcoind <- createSpvNodeFundedWalletFromBitcoind(
        walletCallbacks,
        bip39PasswordOpt,
        bitcoind)
    } yield {
      spvNodeWithBitcoind
    }
  }

  /** Creates a spv node & funded wallet with the given bitcoind */
  def createSpvNodeFundedWalletFromBitcoind(
      walletCallbacks: WalletCallbacks,
      bip39PasswordOpt: Option[String],
      bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[SpvNodeFundedWalletBitcoind] = {
    import system.dispatcher
    require(appConfig.nodeConf.nodeType == NodeType.SpvNode)
    for {
      peer <- createPeer(bitcoind)
      node <- createSpvNode(peer)(system,
                                  appConfig.chainConf,
                                  appConfig.nodeConf)
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
      _ <- BitcoinSWalletTest.awaitWalletBalances(fundedWallet)(
        appConfig.walletConf,
        system)
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
    require(appConfig.nodeConf.nodeType == NodeType.NeutrinoNode)
    for {
      bitcoind <- BitcoinSFixture.createBitcoindWithFunds(versionOpt)
      node <- createNeutrinoNode(bitcoind)(system,
                                           appConfig.chainConf,
                                           appConfig.nodeConf)
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
      _ <- BitcoinSWalletTest.awaitWalletBalances(fundedWallet)(
        appConfig.walletConf,
        system)
    } yield {
      NeutrinoNodeFundedWalletBitcoind(node = syncedNode,
                                       wallet = fundedWallet.wallet,
                                       bitcoindRpc = fundedWallet.bitcoind,
                                       bip39PasswordOpt = bip39PasswordOpt)
    }
  }

  def createNeutrinoNodeFundedWalletFromBitcoind(
      bip39PasswordOpt: Option[String],
      bitcoind: BitcoindRpcClient,
      walletCallbacks: WalletCallbacks)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[
    NeutrinoNodeFundedWalletBitcoind] = {
    import system.dispatcher
    require(appConfig.nodeConf.nodeType == NodeType.NeutrinoNode)
    for {
      node <- createNeutrinoNode(bitcoind)(system,
                                           appConfig.chainConf,
                                           appConfig.nodeConf)
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
      _ <- BitcoinSWalletTest.awaitWalletBalances(fundedWallet)(
        appConfig.walletConf,
        system)
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
                          node = buildNode(peer, chainApi),
                          peer = peer)
    Future.successful(receiver)
  }

  def createPeer(bitcoind: BitcoindRpcClient)(implicit
      executionContext: ExecutionContext): Future[Peer] = {
    NodeTestUtil.getBitcoindPeer(bitcoind)
  }

  def emptyPeer: Peer = {
    val socket = new InetSocketAddress(RpcUtil.randomPort)
    Peer(id = None, socket = socket, socks5ProxyParams = None)
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

    for {
      _ <- checkConfigF
      _ <- nodeAppConfig.start()
      chainHandler <- ChainUnitTest.createChainHandler()
    } yield {
      val dmh = DataMessageHandler(chainHandler)
      SpvNode(
        configPeersOverride = Vector(peer),
        dataMessageHandler = dmh,
        nodeConfig = nodeAppConfig,
        chainConfig = chainAppConfig,
        actorSystem = system
      ).setBloomFilter(P2PMessageTestUtil.emptyBloomFilter)
    }
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
    val nodeF = for {
      _ <- nodeAppConfig.start()
      peer <- createPeer(bitcoind)
      chainApi <- chainApiF
    } yield {
      val dmh = DataMessageHandler(chainApi)
      NeutrinoNode(configPeersOverride = Vector(peer),
                   dataMessageHandler = dmh,
                   nodeConfig = nodeAppConfig,
                   chainConfig = chainAppConfig,
                   actorSystem = system)
    }

    nodeF
  }

  /** Creates a Neutrino node peered with the given bitcoind client, this method
    * also calls [[org.bitcoins.node.Node.start() start]] to start the node
    */
  def createNeutrinoNode(bitcoinds: Vector[BitcoindRpcClient])(implicit
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
    val peersF = bitcoinds.map(createPeer(_))
    val nodeF = for {
      _ <- nodeAppConfig.start()
      chainApi <- chainApiF
      peers <- Future.sequence(peersF)
    } yield {
      val dmh = DataMessageHandler(chainApi)
      NeutrinoNode(configPeersOverride = peers,
                   dataMessageHandler = dmh,
                   nodeConfig = nodeAppConfig,
                   chainConfig = chainAppConfig,
                   actorSystem = system)
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
