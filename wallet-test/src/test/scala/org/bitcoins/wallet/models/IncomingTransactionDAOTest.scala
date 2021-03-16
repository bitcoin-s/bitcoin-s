package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db._
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.wallet.WalletTestUtil

class IncomingTransactionDAOTest extends WalletDAOFixture {

  val txDb: TransactionDb =
    TransactionDbHelper.fromTransaction(WalletTestUtil.sampleTransaction,
                                        Some(DoubleSha256DigestBE.empty))

  val incoming: IncomingTransactionDb =
    IncomingTransactionDb(WalletTestUtil.sampleTransaction.txIdBE,
                          Satoshis(10000))

  it should "insert and read an transaction into the database" in { daos =>
    val txDAO = daos.transactionDAO
    val incomingTxDAO = daos.incomingTxDAO

    for {
      _ <- txDAO.create(txDb)
      created <- incomingTxDAO.create(incoming)
      found <- incomingTxDAO.read(incoming.txIdBE)
    } yield assert(found.contains(created))
  }

  it must "find a transaction by txIdBE" in { daos =>
    val txDAO = daos.transactionDAO
    val incomingTxDAO = daos.incomingTxDAO

    for {
      _ <- txDAO.create(txDb)
      created <- incomingTxDAO.create(incoming)
      found <- incomingTxDAO.findByTxId(incoming.txIdBE)
    } yield assert(found.contains(created))
  }

  it must "find a transaction by txId" in { daos =>
    val txDAO = daos.transactionDAO
    val incomingTxDAO = daos.incomingTxDAO

    for {
      _ <- txDAO.create(txDb)
      created <- incomingTxDAO.create(incoming)
      found <- incomingTxDAO.findByTxId(incoming.txId)
    } yield assert(found.contains(created))
  }
}
