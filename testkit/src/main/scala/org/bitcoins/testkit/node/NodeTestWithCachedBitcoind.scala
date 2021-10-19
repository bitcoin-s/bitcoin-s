package org.bitcoins.testkit.node

import akka.actor.ActorSystem
import org.bitcoins.core.api.node.NodeType
import org.bitcoins.node.Node
import org.bitcoins.node.models.Peer
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.node.NodeUnitTest.{createPeer, syncNeutrinoNode}
import org.bitcoins.testkit.node.fixture.{
  NeutrinoNodeConnectedWithBitcoind,
  NeutrinoNodeConnectedWithBitcoinds,
  SpvNodeConnectedWithBitcoind,
  SpvNodeConnectedWithBitcoindV19
}
import org.bitcoins.testkit.rpc.{
  CachedBitcoind,
  CachedBitcoindNewest,
  CachedBitcoindPairV22,
  CachedBitcoindV19
}
import org.bitcoins.testkit.tor.CachedTor
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.WalletCallbacks
import org.scalatest.FutureOutcome

import scala.concurrent.Future

/** Test trait for using a bitcoin-s [[Node]] that requires a cached bitcoind.
  * The cached bitcoind will be share across tests in the test suite that extends
  * this trait.
  */
trait NodeTestWithCachedBitcoind extends BaseNodeTest with CachedTor {
  _: CachedBitcoind[_] =>

  def withSpvNodeFundedWalletBitcoindCached(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String],
      bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {

    makeDependentFixture[SpvNodeFundedWalletBitcoind](
      build = () =>
        NodeUnitTest.createSpvNodeFundedWalletFromBitcoind(
          walletCallbacks = WalletCallbacks.empty,
          bip39PasswordOpt = bip39PasswordOpt,
          bitcoind = bitcoind)(
          system, // Force V18 because Spv is disabled on versions after
          appConfig),
      { x: SpvNodeFundedWalletBitcoind =>
        tearDownNodeWithBitcoind(x)
      }
    )(test)
  }

  def withSpvNodeConnectedToBitcoindCached(
      test: OneArgAsyncTest,
      bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    val nodeWithBitcoindBuilder: () => Future[SpvNodeConnectedWithBitcoind] = {
      () =>
        require(appConfig.nodeConf.nodeType == NodeType.SpvNode)
        for {
          peer <- createPeer(bitcoind)
          node <- NodeUnitTest.createSpvNode(peer)(system,
                                                   appConfig.chainConf,
                                                   appConfig.nodeConf)
          started <- node.start()
          _ <- NodeUnitTest.syncSpvNode(started, bitcoind)
        } yield SpvNodeConnectedWithBitcoind(node, bitcoind)
    }

    makeDependentFixture[SpvNodeConnectedWithBitcoind](
      build = nodeWithBitcoindBuilder,
      { x: SpvNodeConnectedWithBitcoind =>
        NodeUnitTest.destroyNode(x.node)
      })(test)
  }

  def withNeutrinoNodeUnstarted(
      test: OneArgAsyncTest,
      bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    val nodeWithBitcoindBuilder: () => Future[
      NeutrinoNodeConnectedWithBitcoind] = { () =>
      require(appConfig.nodeConf.nodeType == NodeType.NeutrinoNode)
      for {
        node <- NodeUnitTest.createNeutrinoNode(bitcoind)(system,
                                                          appConfig.chainConf,
                                                          appConfig.nodeConf)
      } yield NeutrinoNodeConnectedWithBitcoind(node, bitcoind)
    }
    makeDependentFixture[NeutrinoNodeConnectedWithBitcoind](
      build = nodeWithBitcoindBuilder,
      { x: NeutrinoNodeConnectedWithBitcoind =>
        tearDownNode(x.node)
      })(test)
  }

  def withNeutrinoNodeConnectedToBitcoinds(
      test: OneArgAsyncTest,
      bitcoinds: Vector[BitcoindRpcClient])(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    val nodeWithBitcoindBuilder: () => Future[
      NeutrinoNodeConnectedWithBitcoinds] = { () =>
      require(appConfig.nodeConf.nodeType == NodeType.NeutrinoNode)
      for {
        node <- NodeUnitTest.createNeutrinoNode(bitcoinds)(system,
                                                           appConfig.chainConf,
                                                           appConfig.nodeConf)
        startedNode <- node.start()
        syncedNode <- syncNeutrinoNode(startedNode, bitcoinds(0))
      } yield NeutrinoNodeConnectedWithBitcoinds(syncedNode, bitcoinds)
    }
    makeDependentFixture[NeutrinoNodeConnectedWithBitcoinds](
      build = nodeWithBitcoindBuilder,
      { x: NeutrinoNodeConnectedWithBitcoinds =>
        tearDownNode(x.node)
      })(test)
  }

  def withNeutrinoNodeFundedWalletBitcoind(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String],
      bitcoind: BitcoindRpcClient,
      walletCallbacks: WalletCallbacks = WalletCallbacks.empty)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    makeDependentFixture[NeutrinoNodeFundedWalletBitcoind](
      build = () =>
        NodeUnitTest
          .createNeutrinoNodeFundedWalletFromBitcoind(
            bip39PasswordOpt = bip39PasswordOpt,
            bitcoind,
            walletCallbacks = walletCallbacks)(system, appConfig),
      { x: NeutrinoNodeFundedWalletBitcoind =>
        tearDownNodeWithBitcoind(x)
      }
    )(test)
  }

  def withBitcoindPeer(
      test: OneArgAsyncTest,
      bitcoind: BitcoindRpcClient): FutureOutcome = {
    makeDependentFixture[Peer](
      () => NodeTestUtil.getBitcoindPeer(bitcoind),
      _ => Future.unit
    )(test)
  }

  private def tearDownNodeWithBitcoind(
      nodeWithBitcoind: NodeFundedWalletBitcoind): Future[Unit] = {
    val node = nodeWithBitcoind.node
    val destroyNodeF = tearDownNode(node)
    val destroyWalletF =
      BitcoinSWalletTest.destroyWallet(nodeWithBitcoind.wallet)
    for {
      _ <- destroyNodeF
      _ <- destroyWalletF
    } yield ()
  }

  private def tearDownNode(node: Node): Future[Unit] = {
    val destroyNodeF = NodeUnitTest.destroyNode(node)
    for {
      _ <- destroyNodeF
      //need to stop chainAppConfig too since this is a test
      _ <- node.chainAppConfig.stop()
    } yield ()
  }
}

trait NodeTestWithCachedBitcoindNewest
    extends NodeTestWithCachedBitcoind
    with CachedBitcoindNewest {

  override def afterAll(): Unit = {
    super[CachedBitcoindNewest].afterAll()
    super[NodeTestWithCachedBitcoind].afterAll()
  }
}

trait NodeTestWithCachedBitcoindPair
    extends NodeTestWithCachedBitcoind
    with CachedBitcoindPairV22 {

  override def afterAll(): Unit = {
    super[CachedBitcoindPairV22].afterAll()
    super[NodeTestWithCachedBitcoind].afterAll()
  }
}

trait NodeTestWithCachedBitcoindV19
    extends NodeTestWithCachedBitcoind
    with CachedBitcoindV19 {

  def withSpvNodeConnectedToBitcoindV19Cached(
      test: OneArgAsyncTest,
      bitcoind: BitcoindV19RpcClient)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    val nodeWithBitcoindBuilder: () => Future[
      SpvNodeConnectedWithBitcoindV19] = { () =>
      require(appConfig.nodeConf.nodeType == NodeType.SpvNode)
      for {
        peer <- createPeer(bitcoind)
        node <- NodeUnitTest.createSpvNode(peer)(system,
                                                 appConfig.chainConf,
                                                 appConfig.nodeConf)
        started <- node.start()
        _ <- NodeUnitTest.syncSpvNode(started, bitcoind)
      } yield SpvNodeConnectedWithBitcoindV19(node, bitcoind)
    }

    makeDependentFixture[SpvNodeConnectedWithBitcoindV19](
      build = nodeWithBitcoindBuilder,
      { x: SpvNodeConnectedWithBitcoindV19 =>
        NodeUnitTest.destroyNode(x.node)
      }
    )(test)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindV19].afterAll()
    super[NodeTestWithCachedBitcoind].afterAll()
  }
}
