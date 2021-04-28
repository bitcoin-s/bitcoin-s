package org.bitcoins.node.models

import org.bitcoins.testkit.fixtures.NodeDAOFixture
import org.bitcoins.testkitcore.Implicits._
import org.bitcoins.testkitcore.gen.TransactionGenerators

class BroadcastAbleTransactionDAOTest extends NodeDAOFixture {

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
