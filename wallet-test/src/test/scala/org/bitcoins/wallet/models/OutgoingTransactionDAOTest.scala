package org.bitcoins.wallet.models

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}

class OutgoingTransactionDAOTest
    extends BitcoinSWalletTest
    with WalletDAOFixture {

  val txDb: TransactionDb =
    TransactionDb.fromTransaction(WalletTestUtil.sampleTransaction)

  val outgoing: OutgoingTransactionDb = OutgoingTransactionDb.fromTransaction(
    WalletTestUtil.sampleTransaction,
    Satoshis(250000000),
    WalletTestUtil.sampleTransaction.outputs.head.value,
    Satoshis(10000))

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
