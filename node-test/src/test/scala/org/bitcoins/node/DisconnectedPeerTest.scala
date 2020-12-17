package org.bitcoins.node

import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.core.gen.TransactionGenerators
import org.bitcoins.testkit.node.NodeUnitTest
import org.scalatest.FutureOutcome

class DisconnectedPeerTest extends NodeUnitTest {

  /** Wallet config with data directory set to user temp directory */
  implicit override protected val config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = SpvNode

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withDisconnectedSpvNode(test)

  it must "fail to broadcast a transaction when disconnected" in { node =>
    val tx = TransactionGenerators.transaction.sampleSome
    recoverToSucceededIf[RuntimeException] {
      node.broadcastTransaction(tx)
    }
  }
}
