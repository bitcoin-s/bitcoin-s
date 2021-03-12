package org.bitcoins.testkit.node

import akka.actor.ActorSystem
import org.bitcoins.node.NodeType
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.node.NodeUnitTest.createPeer
import org.bitcoins.testkit.node.fixture.SpvNodeConnectedWithBitcoindV19
import org.bitcoins.testkit.rpc.{
  CachedBitcoind,
  CachedBitcoindNewest,
  CachedBitcoindV19
}
import org.bitcoins.wallet.WalletCallbacks
import org.scalatest.FutureOutcome

import scala.concurrent.Future

trait NodeTestWithCachedBitcoind extends BaseNodeTest { _: CachedBitcoind =>

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
        NodeUnitTest.destroyNode(x.node)
      }
    )(test)
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
