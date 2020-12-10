package org.bitcoins.node

import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.core.gen.TransactionGenerators
import org.bitcoins.testkit.node.NodeUnitTest
import org.bitcoins.testkit.node.fixture.SpvNodeConnectedWithBitcoind
import org.scalatest.FutureOutcome

import scala.concurrent.duration._

class DisconnectedPeerTest extends NodeUnitTest {

  /** Wallet config with data directory set to user temp directory */
  implicit override protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = SpvNodeConnectedWithBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withSpvNodeConnectedToBitcoind(test, Some(BitcoindVersion.V17))

  it must "fail to broadcast a transaction when disconnected" in { param =>
    val SpvNodeConnectedWithBitcoind(node, rpc) = param

    val tx = TransactionGenerators.transaction.sampleSome

    for {
      peers <- rpc.getPeerInfo
      me = peers.head
      _ <- rpc.disconnectNode(me.networkInfo.addr)

      // Wait until disconnected
      _ <-
        TestAsyncUtil.retryUntilSatisfiedF(() => rpc.getPeerInfo.map(_.isEmpty),
                                           500.millis)

      res <- recoverToSucceededIf[RuntimeException] {
        node.broadcastTransaction(tx)
      }
    } yield res
  }
}
