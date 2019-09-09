package org.bitcoins.testkit.node

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.bitcoins.node.{NeutrinoNode, Node, P2PLogger, SpvNode, SpvNodeCallbacks}
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.db.AppConfig
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.{PeerHandler, PeerMessageReceiver, PeerMessageReceiverState, PeerMessageSender}
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.BitcoinSAppConfig._
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.chain.ChainUnitTest
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.node.fixture.NodeConnectedWithBitcoind
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.api.UnlockedWalletApi
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, FutureOutcome, MustMatchers}
import java.net.InetSocketAddress

import org.bitcoins.testkit.node.NodeUnitTest.NodeFundedWalletBitcoind

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

trait NodeUnitTest
    extends BitcoinSFixture
    with MustMatchers
    with P2PLogger
    with BeforeAndAfter
    with BeforeAndAfterAll {

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(config.nodeConf)
  }

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system, verifySystemShutdown = true)
    ()
  }

  implicit lazy val system: ActorSystem = {
    ActorSystem(s"${getClass.getSimpleName}-${System.currentTimeMillis}")
  }

  implicit lazy val ec: ExecutionContext =
    system.dispatcher

  val timeout: FiniteDuration = 10.seconds

  /** Wallet config with data directory set to user temp directory */
  implicit protected def config: BitcoinSAppConfig

  implicit protected lazy val chainConfig: ChainAppConfig = config.chainConf

  implicit protected lazy val nodeConfig: NodeAppConfig = config.nodeConf

  implicit lazy val np: NetworkParameters = config.nodeConf.network

  lazy val startedBitcoindF = BitcoindRpcTestUtil.startedBitcoindRpcClient()

  lazy val bitcoindPeerF = startedBitcoindF.map(NodeTestUtil.getBitcoindPeer)

  def createNode(bitcoind: BitcoindRpcClient, callbacks: SpvNodeCallbacks)(
    implicit system: ActorSystem,
    chainAppConfig: ChainAppConfig,
    nodeAppConfig: NodeAppConfig): Future[Node] = {
    if (nodeAppConfig.isNeutrinoEnabled)
      NodeUnitTest.createNeutrinoNode(bitcoind, callbacks)(system, chainAppConfig, nodeAppConfig)
    else
      NodeUnitTest.createSpvNode(bitcoind, callbacks)(system, chainAppConfig, nodeAppConfig)
  }

  def withNode(test: OneArgAsyncTest)(
      implicit system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {

    val nodeBuilder: () => Future[Node] = { () =>
      val bitcoindF = BitcoinSFixture.createBitcoind()
      bitcoindF.flatMap { bitcoind =>
          createNode(bitcoind, SpvNodeCallbacks.empty)(system, appConfig.chainConf, appConfig.nodeConf)
          .flatMap(_.start())
      }
    }

    makeDependentFixture(
      build = nodeBuilder,
      destroy =
        NodeUnitTest.destroyNode(_: Node)(appConfig, system.dispatcher)
    )(test)
  }

  def withNodeConnectedToBitcoind(test: OneArgAsyncTest, versionOpt: Option[BitcoindVersion] = None)(
      implicit system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    val nodeWithBitcoindBuilder: () => Future[NodeConnectedWithBitcoind] = {
      () =>
        val bitcoindF = BitcoinSFixture.createBitcoind(versionOpt)
        bitcoindF.flatMap { bitcoind =>
          val nodeF =
            createNode(bitcoind, SpvNodeCallbacks.empty)(
              system,
              appConfig.chainConf,
              appConfig.nodeConf)

          nodeF.map(spv => NodeConnectedWithBitcoind(spv, bitcoind))
        }
    }

    makeDependentFixture(
      build = nodeWithBitcoindBuilder,
      destroy = NodeUnitTest.destroyNodeConnectedWithBitcoind(
        _: NodeConnectedWithBitcoind)(system, appConfig)
    )(test)
  }

  def withNodeFundedWalletBitcoind(
      test: OneArgAsyncTest,
      callbacks: SpvNodeCallbacks)(
      implicit system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {

    makeDependentFixture(
      build = () =>
        NodeUnitTest.createNodeFundedWalletBitcoind(callbacks)(system,
          appConfig),
      destroy = NodeUnitTest.destroyNodeFundedWalletBitcoind(
        _: NodeFundedWalletBitcoind)(system, appConfig)
    )(test)
  }
}

object NodeUnitTest extends P2PLogger {

  /**
    * Creates
    * 1. a funded bitcoind wallet
    * 2. a funded bitcoin-s wallet
    * 3. a chain handler with the appropriate tables created
    * 4. a spv node that is connected to the bitcoin instance -- but not started!  */
  case class NodeFundedWalletBitcoind(
      node: Node,
      wallet: UnlockedWalletApi,
      bitcoindRpc: BitcoindRpcClient) {
    def spvNode: SpvNode = node.asInstanceOf[SpvNode]
    def neutrinoNode: NeutrinoNode = node.asInstanceOf[NeutrinoNode]
  }

  def buildPeerMessageReceiver(chainApi: ChainApi, peer: Peer)(
      implicit appConfig: BitcoinSAppConfig,
      system: ActorSystem): Future[PeerMessageReceiver] = {
    val receiver =
      PeerMessageReceiver(state = PeerMessageReceiverState.fresh(),
                          chainApi = chainApi,
                          peer = peer,
                          callbacks = SpvNodeCallbacks.empty)
    Future.successful(receiver)
  }

  def buildPeerHandler(peer: Peer)(
      implicit nodeAppConfig: NodeAppConfig,
      chainAppConfig: ChainAppConfig,
      system: ActorSystem): Future[PeerHandler] = {
    import system.dispatcher
    val chainApiF = ChainUnitTest.createChainHandler()
    val peerMsgReceiverF = chainApiF.flatMap { _ =>
      PeerMessageReceiver.preConnection(peer, SpvNodeCallbacks.empty)
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

  def destroyNode(node: Node)(
      implicit config: BitcoinSAppConfig,
      ec: ExecutionContext): Future[Unit] = {
    node.stop()
      .flatMap(_ => ChainUnitTest.destroyHeaderTable())
      .flatMap(_ => ChainUnitTest.destroyFilterHeaderTable())
      .flatMap(_ => ChainUnitTest.destroyFilterTable())
  }

  def destroyNodeConnectedWithBitcoind(
      nodeConnectedWithBitcoind: NodeConnectedWithBitcoind)(
      implicit system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[Unit] = {
    logger.debug(s"Beggining tear down of spv node connected with bitcoind")
    import system.dispatcher
    val node = nodeConnectedWithBitcoind.node
    val bitcoind = nodeConnectedWithBitcoind.bitcoind
    val resultF = for {
      _ <- destroyNode(node)
      _ <- ChainUnitTest.destroyBitcoind(bitcoind)
    } yield {
      logger.debug(s"Done with teardown of spv node connected with bitcoind!")
      ()
    }

    resultF
  }

  /** Creates a spv node, a funded bitcoin-s wallet, all of which are connected to bitcoind */
  def createNodeFundedWalletBitcoind(callbacks: SpvNodeCallbacks)(
      implicit system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[NodeFundedWalletBitcoind] = {
    import system.dispatcher
    val fundedWalletF = BitcoinSWalletTest.fundedWalletAndBitcoind()
    for {
      fundedWallet <- fundedWalletF
      spvNode <- createSpvNode(fundedWallet.bitcoind, callbacks)
    } yield {
      NodeFundedWalletBitcoind(node = spvNode,
                                  wallet = fundedWallet.wallet,
                                  bitcoindRpc = fundedWallet.bitcoind)
    }
  }

  def destroyNodeFundedWalletBitcoind(
      fundedWalletBitcoind: NodeFundedWalletBitcoind)(
      implicit system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[Unit] = {
    import system.dispatcher
    val walletWithBitcoind = {
      BitcoinSWalletTest.WalletWithBitcoind(fundedWalletBitcoind.wallet,
                                            fundedWalletBitcoind.bitcoindRpc)
    }

    //these need to be done in order, as the spv node needs to be
    //stopped before the bitcoind node is stopped
    val destroyedF = for {
      _ <- destroyNode(fundedWalletBitcoind.node)
      _ <- BitcoinSWalletTest.destroyWalletWithBitcoind(walletWithBitcoind)
    } yield ()

    destroyedF

  }

  def buildPeerMessageReceiver(chainApi: ChainApi, peer: Peer)(
      implicit nodeAppConfig: NodeAppConfig,
      chainAppConfig: ChainAppConfig,
      system: ActorSystem): Future[PeerMessageReceiver] = {
    val receiver =
      PeerMessageReceiver(state = PeerMessageReceiverState.fresh(),
                          chainApi = chainApi,
                          peer = peer,
                          callbacks = SpvNodeCallbacks.empty)
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

  /** Creates a spv node peered with the given bitcoind client, this method
    * also calls [[org.bitcoins.node.Node.start() start]] to start the node */
  def createSpvNode(bitcoind: BitcoindRpcClient, callbacks: SpvNodeCallbacks)(
      implicit system: ActorSystem,
      chainAppConfig: ChainAppConfig,
      nodeAppConfig: NodeAppConfig): Future[Node] = {
    import system.dispatcher
    val checkConfigF = Future {
      assert(nodeAppConfig.isSPVEnabled)
      assert(!nodeAppConfig.isNeutrinoEnabled)
    }
    val chainApiF = for {
      _ <- checkConfigF
      chainHandler <- ChainUnitTest.createChainHandler()
    } yield chainHandler
    val peer = createPeer(bitcoind)
    val nodeF = for {
      _ <- chainApiF
    } yield {
      SpvNode(nodePeer = peer,
        bloomFilter = NodeTestUtil.emptyBloomFilter,
        nodeCallbacks = callbacks,
        nodeConfig = nodeAppConfig,
        chainConfig = chainAppConfig,
        actorSystem = system
      )
    }

    nodeF.flatMap(_.start())
  }

  /** Creates a Neutrino node peered with the given bitcoind client, this method
    * also calls [[org.bitcoins.node.Node.start() start]] to start the node */
  def createNeutrinoNode(bitcoind: BitcoindRpcClient, callbacks: SpvNodeCallbacks)(
    implicit system: ActorSystem,
    chainAppConfig: ChainAppConfig,
    nodeAppConfig: NodeAppConfig): Future[Node] = {
    import system.dispatcher
    val checkConfigF = Future {
      assert(!nodeAppConfig.isSPVEnabled)
      assert(nodeAppConfig.isNeutrinoEnabled)
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
        nodeCallbacks = callbacks,
        nodeConfig = nodeAppConfig,
        chainConfig = chainAppConfig,
        actorSystem = system
      )
    }

    nodeF.flatMap(_.start())
  }

}
