package org.bitcoins.node

import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkitcore.Implicits._
import org.bitcoins.testkitcore.gen.TransactionGenerators
import org.bitcoins.testkit.node.{CachedBitcoinSAppConfig, NodeUnitTest}
import org.scalatest.FutureOutcome

class DisconnectedPeerTest extends NodeUnitTest with CachedBitcoinSAppConfig {

  implicit override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvWithEmbeddedDbTestConfig(pgUrl)

  /** Wallet config with data directory set to user temp directory */
  implicit override protected lazy val cachedConfig: BitcoinSAppConfig =
    getFreshConfig

  override type FixtureParam = SpvNode

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withDisconnectedSpvNode(test)(system, cachedConfig)

  it must "fail to broadcast a transaction when disconnected" in { node =>
    val tx = TransactionGenerators.transaction.sampleSome
    recoverToSucceededIf[RuntimeException] {
      node.broadcastTransaction(tx)
    }
  }
}
