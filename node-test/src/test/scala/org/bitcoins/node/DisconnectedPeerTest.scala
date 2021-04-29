package org.bitcoins.node

import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.NodeUnitTest
import org.bitcoins.testkitcore.Implicits._
import org.bitcoins.testkitcore.gen.TransactionGenerators
import org.scalatest.FutureOutcome

class DisconnectedPeerTest extends NodeUnitTest {

  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvWithEmbeddedDbTestConfig(pgUrl, Vector.empty)

  override type FixtureParam = SpvNode

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withDisconnectedSpvNode(test)(system, getFreshConfig)

  it must "fail to broadcast a transaction when disconnected" in { node =>
    val tx = TransactionGenerators.transaction.sampleSome
    recoverToSucceededIf[RuntimeException] {
      node.broadcastTransaction(tx)
    }
  }
}
