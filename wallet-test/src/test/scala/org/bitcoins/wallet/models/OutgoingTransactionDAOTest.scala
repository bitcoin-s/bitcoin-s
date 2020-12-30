package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.{
  OutgoingTransactionDb,
  TransactionDb,
  TransactionDbHelper
}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.testkit.Implicits.GeneratorOps
import org.bitcoins.testkit.core.gen.TransactionGenerators
import org.bitcoins.testkit.fixtures.WalletDAOFixture

class OutgoingTransactionDAOTest extends WalletDAOFixture {

  val tx: Transaction = TransactionGenerators.realisticTransaction.sampleSome

  val txDb: TransactionDb =
    TransactionDbHelper.fromTransaction(tx)

  val outgoing: OutgoingTransactionDb = OutgoingTransactionDb.fromTransaction(
    tx = tx,
    inputAmount = tx.outputs.head.value,
    sentAmount = tx.outputs.head.value,
    expectedFee = Satoshis.zero)

  it should "insert and read an transaction into the database" in { daos =>
    val txDAO = daos.transactionDAO
    val outgoingTxDAO = daos.outgoingTxDAO

    for {
      _ <- txDAO.create(txDb)
      created <- outgoingTxDAO.create(outgoing)
      found <- outgoingTxDAO.read(outgoing.txIdBE)
    } yield assert(found.contains(created))
  }

  it must "find a transaction by txIdBE" in { daos =>
    val txDAO = daos.transactionDAO
    val outgoingTxDAO = daos.outgoingTxDAO

    for {
      _ <- txDAO.create(txDb)
      created <- outgoingTxDAO.create(outgoing)
      found <- outgoingTxDAO.findByTxId(outgoing.txIdBE)
    } yield assert(found.contains(created))
  }

  it must "find a transaction by txId" in { daos =>
    val txDAO = daos.transactionDAO
    val outgoingTxDAO = daos.outgoingTxDAO

    for {
      _ <- txDAO.create(txDb)
      created <- outgoingTxDAO.create(outgoing)
      found <- outgoingTxDAO.findByTxId(outgoing.txId)
    } yield assert(found.contains(created))
  }
}
