package org.bitcoins.wallet.models

import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}

class IncomingTransactionDAOTest
    extends BitcoinSWalletTest
    with WalletDAOFixture {

  val txDb: IncomingTransactionDb =
    IncomingTransactionDb.fromTransaction(WalletTestUtil.sampleTransaction)

  it should "insert and read an transaction into the database" in { daos =>
    val txDAO = daos.incomingTxDAO

    for {
      created <- txDAO.create(txDb)
      found <- txDAO.read(txDb.txIdBE)
    } yield assert(found.contains(created))
  }

  it must "find a transaction by txIdBE" in { daos =>
    val txDAO = daos.incomingTxDAO

    for {
      created <- txDAO.create(txDb)
      found <- txDAO.findByTxId(txDb.txIdBE)
    } yield assert(found.contains(created))
  }

  it must "find a transaction by txId" in { daos =>
    val txDAO = daos.incomingTxDAO

    for {
      created <- txDAO.create(txDb)
      found <- txDAO.findByTxId(txDb.txId)
    } yield assert(found.contains(created))
  }
}
