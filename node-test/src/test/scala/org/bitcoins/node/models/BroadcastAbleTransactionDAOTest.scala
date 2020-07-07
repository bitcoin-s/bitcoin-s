package org.bitcoins.node.models

import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.core.gen.TransactionGenerators
import org.bitcoins.testkit.fixtures.NodeDAOFixture
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}

class BroadcastAbleTransactionDAOTest extends NodeDAOFixture with EmbeddedPg {

  /** Wallet config with data directory set to user temp directory */
  implicit override protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvWithEmbeddedDbTestConfig(pgUrl)

  behavior of "BroadcastAbleTransactionDAO"

  it must "write a TX and read it back" in { daos =>
    val txDAO = daos.txDAO
    val tx = TransactionGenerators.transaction.sampleSome

    for {
      created <- txDAO.create(BroadcastAbleTransaction(tx))
      read <- txDAO.read(created.transaction.txIdBE)
    } yield assert(read.contains(created))

  }
}
