package org.bitcoins.testkit.node

import akka.actor.ActorSystem
import org.bitcoins.node.{Node, NodeType}
import org.bitcoins.node.models.Peer
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.chain.ChainUnitTest
import org.bitcoins.testkit.node.NodeUnitTest.{createPeer, syncNeutrinoNode}
import org.bitcoins.testkit.node.fixture.{
  NeutrinoNodeConnectedWithBitcoind,
  SpvNodeConnectedWithBitcoind,
  SpvNodeConnectedWithBitcoindV19
}
import org.bitcoins.testkit.rpc.{
  CachedBitcoind,
  CachedBitcoindNewest,
  CachedBitcoindV19
}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.WalletCallbacks
import org.scalatest.FutureOutcome

import scala.concurrent.Future

/** Test trait for using a bitcoin-s [[Node]] that requires a cached bitcoind.
  * The cached bitcoind will be share across tests in the test suite that extends
  * this trait.
  */
trait NodeTestWithCachedBitcoind extends BaseNodeTest { _: CachedBitcoind[_] =>

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
          node <- NodeUnitTest.createSpvNode(createPeer(bitcoind))(
            system,
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

  def withNeutrinoNodeConnectedToBitcoind(
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
        startedNode <- node.start()
        syncedNode <- syncNeutrinoNode(startedNode, bitcoind)
      } yield NeutrinoNodeConnectedWithBitcoind(syncedNode, bitcoind)
    }
    makeDependentFixture[NeutrinoNodeConnectedWithBitcoind](
      build = nodeWithBitcoindBuilder,
      { case x: NeutrinoNodeConnectedWithBitcoind =>
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
    val peer = NodeTestUtil.getBitcoindPeer(bitcoind)

    makeDependentFixture[Peer](
      () => Future.successful(peer),
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
      _ <- ChainUnitTest.destroyAllTables()(node.chainAppConfig,
                                            system.dispatcher)
    } yield ()
  }
}

trait NodeTestWithCachedBitcoindNewest
    extends NodeTestWithCachedBitcoind
    with CachedBitcoindNewest

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
      require(appConfig.nodeType == NodeType.SpvNode)
      for {
        node <- NodeUnitTest.createSpvNode(createPeer(bitcoind))(
          system,
          appConfig.chainConf,
          appConfig.nodeConf)
        started <- node.start()
        _ <- NodeUnitTest.syncSpvNode(started, bitcoind)
      } yield SpvNodeConnectedWithBitcoindV19(node, bitcoind)
    }

    makeDependentFixture[SpvNodeConnectedWithBitcoindV19](
      build = nodeWithBitcoindBuilder,
      { case x: SpvNodeConnectedWithBitcoindV19 =>
        NodeUnitTest.destroyNode(x.node)
      }
    )(test)
  }
}
