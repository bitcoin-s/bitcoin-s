package org.bitcoins.testkit.node

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.AppConfig
import org.bitcoins.node.{SpvNode, SpvNodeCallbacks}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.{
  PeerHandler,
  PeerMessageReceiver,
  PeerMessageReceiverState,
  PeerMessageSender
}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.BitcoinSAppConfig._
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.chain.ChainUnitTest
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.node.NodeUnitTest.SpvNodeFundedWalletBitcoind
import org.bitcoins.testkit.node.fixture.SpvNodeConnectedWithBitcoind
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.api.UnlockedWalletApi
import org.scalatest.{
  BeforeAndAfter,
  BeforeAndAfterAll,
  FutureOutcome,
  MustMatchers
}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

trait NodeUnitTest
    extends BitcoinSFixture
    with MustMatchers
    with BitcoinSLogger
    with BeforeAndAfter
    with BeforeAndAfterAll {

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(config.nodeConf)
  }

  override def afterAll(): Unit = {
    system.terminate()
    ()
  }

  implicit lazy val system: ActorSystem = {
    ActorSystem(s"${getClass.getSimpleName}-${System.currentTimeMillis}")
  }

  implicit lazy val ec: ExecutionContext =
    system.dispatcher

  val timeout: FiniteDuration = 10.seconds

  /** Wallet config with data directory set to user temp directory */
  implicit protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getTestConfig()

  implicit protected lazy val chainConfig: ChainAppConfig = config.chainConf

  implicit protected lazy val nodeConfig: NodeAppConfig = config.nodeConf

  implicit lazy val np: NetworkParameters = config.nodeConf.network

  lazy val startedBitcoindF = BitcoindRpcTestUtil.startedBitcoindRpcClient()

  lazy val bitcoindPeerF = startedBitcoindF.map(NodeTestUtil.getBitcoindPeer)

  def withSpvNode(test: OneArgAsyncTest)(
      implicit system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {

    val spvBuilder: () => Future[SpvNode] = { () =>
      val bitcoindF = BitcoinSFixture.createBitcoind()
      bitcoindF.flatMap { bitcoind =>
        NodeUnitTest
          .createSpvNode(bitcoind, SpvNodeCallbacks.empty)(system,
                                                           appConfig.chainConf,
                                                           appConfig.nodeConf)
          .flatMap(_.start())
      }
    }

    makeDependentFixture(
      build = spvBuilder,
      destroy =
        NodeUnitTest.destroySpvNode(_: SpvNode)(appConfig, system.dispatcher)
    )(test)
  }

  def withSpvNodeConnectedToBitcoind(test: OneArgAsyncTest)(
      implicit system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    val spvWithBitcoindBuilder: () => Future[SpvNodeConnectedWithBitcoind] = {
      () =>
        val bitcoindF = BitcoinSFixture.createBitcoind()
        bitcoindF.flatMap { bitcoind =>
          val spvNode = NodeUnitTest
            .createSpvNode(bitcoind, SpvNodeCallbacks.empty)(
              system,
              appConfig.chainConf,
              appConfig.nodeConf)
          val startedSpv = spvNode
            .flatMap(_.start())

          startedSpv.map(spv => SpvNodeConnectedWithBitcoind(spv, bitcoind))
        }
    }

    makeDependentFixture(
      build = spvWithBitcoindBuilder,
      destroy = NodeUnitTest.destorySpvNodeConnectedWithBitcoind(
        _: SpvNodeConnectedWithBitcoind)(system, appConfig)
    )(test)
  }

  def withSpvNodeFundedWalletBitcoind(
      test: OneArgAsyncTest,
      callbacks: SpvNodeCallbacks)(
      implicit system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {

    makeDependentFixture(
      build = () =>
        NodeUnitTest.createSpvNodeFundedWalletBitcoind(callbacks)(system,
                                                                  appConfig),
      destroy = NodeUnitTest.destroySpvNodeFundedWalletBitcoind(
        _: SpvNodeFundedWalletBitcoind)(system, appConfig)
    )(test)
  }
}

object NodeUnitTest extends BitcoinSLogger {

  /**
    * Creates
    * 1. a funded bitcoind wallet
    * 2. a funded bitcoin-s wallet
    * 3. a chain handler with the appropriate tables created
    * 4. a spv node that is connected to the bitcoin instance -- but not started!  */
  case class SpvNodeFundedWalletBitcoind(
      spvNode: SpvNode,
      wallet: UnlockedWalletApi,
      bitcoindRpc: BitcoindRpcClient)

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

  def destroySpvNode(spvNode: SpvNode)(
      implicit config: BitcoinSAppConfig,
      ec: ExecutionContext): Future[Unit] = {
    val stopF = spvNode.stop()
    stopF.flatMap(_ => ChainUnitTest.destroyHeaderTable())
  }

  def destorySpvNodeConnectedWithBitcoind(
      spvNodeConnectedWithBitcoind: SpvNodeConnectedWithBitcoind)(
      implicit system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[Unit] = {
    logger.debug(s"Beggining tear down of spv node connected with bitcoind")
    import system.dispatcher
    val spvNode = spvNodeConnectedWithBitcoind.spvNode
    val bitcoind = spvNodeConnectedWithBitcoind.bitcoind
    val spvNodeDestroyF = destroySpvNode(spvNode)
    val bitcoindDestroyF = ChainUnitTest.destroyBitcoind(bitcoind)

    for {
      _ <- spvNodeDestroyF
      _ <- bitcoindDestroyF
    } yield {
      logger.debug(s"Done with teardown of spv node connected with bitcoind!")
      ()
    }
  }

  /** Creates a spv node, a funded bitcoin-s wallet, all of which are connected to bitcoind */
  def createSpvNodeFundedWalletBitcoind(callbacks: SpvNodeCallbacks)(
      implicit system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[SpvNodeFundedWalletBitcoind] = {
    import system.dispatcher
    val fundedWalletF = BitcoinSWalletTest.fundedWalletAndBitcoind()
    for {
      fundedWallet <- fundedWalletF
      spvNode <- createSpvNode(fundedWallet.bitcoind, callbacks)
    } yield {
      SpvNodeFundedWalletBitcoind(spvNode = spvNode,
                                  wallet = fundedWallet.wallet,
                                  bitcoindRpc = fundedWallet.bitcoind)
    }
  }

  def destroySpvNodeFundedWalletBitcoind(
      fundedWalletBitcoind: SpvNodeFundedWalletBitcoind)(
      implicit system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[Unit] = {
    import system.dispatcher
    val walletWithBitcoind = {
      BitcoinSWalletTest.WalletWithBitcoind(fundedWalletBitcoind.wallet,
                                            fundedWalletBitcoind.bitcoindRpc)
    }
    val destroyedF = for {
      _ <- destroySpvNode(fundedWalletBitcoind.spvNode)
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

  def createSpvNode(bitcoind: BitcoindRpcClient, callbacks: SpvNodeCallbacks)(
      implicit system: ActorSystem,
      chainAppConfig: ChainAppConfig,
      nodeAppConfig: NodeAppConfig): Future[SpvNode] = {
    import system.dispatcher
    val chainApiF = ChainUnitTest.createChainHandler()
    val peer = createPeer(bitcoind)
    for {
      _ <- chainApiF
    } yield {
      SpvNode(peer = peer,
              bloomFilter = NodeTestUtil.emptyBloomFilter,
              callbacks = callbacks)
    }
  }

}
