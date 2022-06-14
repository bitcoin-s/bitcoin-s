package org.bitcoins.node

import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.NodeUnitTest
import org.bitcoins.testkitcore.Implicits._
import org.bitcoins.testkitcore.gen.TransactionGenerators
import org.scalatest.FutureOutcome

class DisconnectedPeerTest extends NodeUnitTest {

  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = NeutrinoNode

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withDisconnectedNeutrinoNode(test)(system, getFreshConfig)

  it must "fail to broadcast a transaction when disconnected" in { node =>
    val tx = TransactionGenerators.transaction.sampleSome
    recoverToSucceededIf[RuntimeException] {
      node.broadcastTransaction(tx)
    }
  }
}
