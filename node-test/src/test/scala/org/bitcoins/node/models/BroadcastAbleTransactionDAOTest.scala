package org.bitcoins.node.models

import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.core.gen.TransactionGenerators
import org.bitcoins.testkit.fixtures.NodeDAOFixture

class BroadcastAbleTransactionDAOTest extends NodeDAOFixture {

  /** Wallet config with data directory set to user temp directory */
  implicit override protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig()

  behavior of "BroadcastAbleTransactionDAO"

  it must "write a TX and read it back" in { daos =>
    val txDAO = daos.txDAO
    val tx = TransactionGenerators.transaction.sampleSome

    for {
      created <- txDAO.create(BroadcastAbleTransaction(tx))
      read <- txDAO.read(created.id.get)
    } yield assert(read.contains(created))

  }
}
