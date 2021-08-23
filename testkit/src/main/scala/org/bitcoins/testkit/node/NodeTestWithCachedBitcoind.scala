package org.bitcoins.testkit.node

import akka.actor.ActorSystem
import org.bitcoins.node.models.Peer
import org.bitcoins.node.{Node, NodeType}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v21.BitcoindV21RpcClient
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.node.NodeUnitTest.{createPeer, syncNeutrinoNode}
import org.bitcoins.testkit.node.fixture.{
  NeutrinoNodeConnectedWithBitcoind,
  NeutrinoNodeConnectedWithBitcoinds,
  SpvNodeConnectedWithBitcoind,
  SpvNodeConnectedWithBitcoindV21
}
import org.bitcoins.testkit.rpc.{
  CachedBitcoind,
  CachedBitcoindNewest,
  CachedBitcoindPairV21,
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
      { case x: SpvNodeFundedWalletBitcoind =>
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
        require(appConfig.nodeType == NodeType.SpvNode)
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
      { case x: SpvNodeConnectedWithBitcoind =>
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
      require(appConfig.nodeType == NodeType.NeutrinoNode)
      for {
        node <- NodeUnitTest.createNeutrinoNode(bitcoind)(system,
                                                          appConfig.chainConf,
                                                          appConfig.nodeConf)
      } yield NeutrinoNodeConnectedWithBitcoind(node, bitcoind)
    }
    makeDependentFixture[NeutrinoNodeConnectedWithBitcoind](
      build = nodeWithBitcoindBuilder,
      { case x: NeutrinoNodeConnectedWithBitcoind =>
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
      require(appConfig.nodeType == NodeType.NeutrinoNode)
      for {
        node <- NodeUnitTest.createNeutrinoNode(bitcoinds)(system,
                                                           appConfig.chainConf,
                                                           appConfig.nodeConf)
        startedNode <- node.start()
        //is it enough to just sync with one bitcoind client for a test?
        syncedNode <- syncNeutrinoNode(startedNode, bitcoinds(0))
      } yield NeutrinoNodeConnectedWithBitcoinds(syncedNode, bitcoinds)
    }
    makeDependentFixture[NeutrinoNodeConnectedWithBitcoinds](
      build = nodeWithBitcoindBuilder,
      { case x: NeutrinoNodeConnectedWithBitcoinds =>
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
      { case x: NeutrinoNodeFundedWalletBitcoind =>
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
    with CachedBitcoindPairV21 {

  override def afterAll(): Unit = {
    super[CachedBitcoindPairV21].afterAll()
    super[NodeTestWithCachedBitcoind].afterAll()
  }
}

trait NodeTestWithCachedBitcoindV19
    extends NodeTestWithCachedBitcoind
    with CachedBitcoindV19 {

  def withSpvNodeConnectedToBitcoindV19Cached(
      test: OneArgAsyncTest,
      bitcoind: BitcoindV21RpcClient)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    val nodeWithBitcoindBuilder: () => Future[
      SpvNodeConnectedWithBitcoindV21] = { () =>
      require(appConfig.nodeType == NodeType.SpvNode)
      for {
        peer <- createPeer(bitcoind)
        node <- NodeUnitTest.createSpvNode(peer)(system,
                                                 appConfig.chainConf,
                                                 appConfig.nodeConf)
        started <- node.start()
        _ <- NodeUnitTest.syncSpvNode(started, bitcoind)
      } yield SpvNodeConnectedWithBitcoindV21(node, bitcoind)
    }

    makeDependentFixture[SpvNodeConnectedWithBitcoindV21](
      build = nodeWithBitcoindBuilder,
      { case x: SpvNodeConnectedWithBitcoindV21 =>
        NodeUnitTest.destroyNode(x.node)
      }
    )(test)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindV19].afterAll()
    super[NodeTestWithCachedBitcoind].afterAll()
  }
}
