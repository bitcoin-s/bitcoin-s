package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.{TransactionDb, TransactionDbHelper}
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.wallet.WalletTestUtil

class TransactionDAOTest extends WalletDAOFixture {

  val txDb: TransactionDb =
    TransactionDbHelper.fromTransaction(WalletTestUtil.sampleTransaction)

  it should "insert and read an transaction into the database" in { daos =>
    val txDAO = daos.transactionDAO

    for {
      created <- txDAO.create(txDb)
      found <- txDAO.read(txDb.txIdBE)
    } yield assert(found.contains(created))
  }

  it must "find a transaction by txIdBE" in { daos =>
    val txDAO = daos.transactionDAO

    for {
      created <- txDAO.create(txDb)
      found <- txDAO.findByTxId(txDb.txIdBE)
    } yield assert(found.contains(created))
  }

  it must "find a transaction by txId" in { daos =>
    val txDAO = daos.transactionDAO

    for {
      created <- txDAO.create(txDb)
      found <- txDAO.findByTxId(txDb.txId)
    } yield assert(found.contains(created))
  }
}
