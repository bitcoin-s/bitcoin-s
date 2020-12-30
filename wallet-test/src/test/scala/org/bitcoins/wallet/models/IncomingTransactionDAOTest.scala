package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.{
  IncomingTransactionDb,
  TransactionDb,
  TransactionDbHelper
}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.testkit.Implicits.GeneratorOps
import org.bitcoins.testkit.core.gen.TransactionGenerators
import org.bitcoins.testkit.fixtures.WalletDAOFixture

class IncomingTransactionDAOTest extends WalletDAOFixture {

  val tx: Transaction = TransactionGenerators.realisticTransaction.sampleSome

  val txDb: TransactionDb = TransactionDbHelper.fromTransaction(tx)

  val incoming: IncomingTransactionDb =
    IncomingTransactionDb(tx.txIdBE, Satoshis(10000))

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
