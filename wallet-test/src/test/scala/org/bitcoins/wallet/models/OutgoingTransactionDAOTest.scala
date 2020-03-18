package org.bitcoins.wallet.models

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}

class OutgoingTransactionDAOTest
    extends BitcoinSWalletTest
    with WalletDAOFixture {

  val txDb: OutgoingTransactionDb = OutgoingTransactionDb.fromTransaction(
    WalletTestUtil.sampleTransaction,
    SatoshisPerVirtualByte(64),
    Satoshis(250000000))

  it should "insert and read an transaction into the database" in { daos =>
    val txDAO = daos.outgoingTxDAO

    println(txDb)

    for {
      created <- txDAO.create(txDb)
      found <- txDAO.read(txDb.txIdBE)
    } yield assert(found.contains(created))
  }

  it must "find a transaction by txIdBE" in { daos =>
    val txDAO = daos.outgoingTxDAO

    for {
      created <- txDAO.create(txDb)
      found <- txDAO.findByTxId(txDb.txIdBE)
    } yield assert(found.contains(created))
  }

  it must "find a transaction by txId" in { daos =>
    val txDAO = daos.outgoingTxDAO

    for {
      created <- txDAO.create(txDb)
      found <- txDAO.findByTxId(txDb.txId)
    } yield assert(found.contains(created))
  }
}
