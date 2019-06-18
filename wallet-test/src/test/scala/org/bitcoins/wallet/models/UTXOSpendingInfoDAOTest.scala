package org.bitcoins.wallet.models

import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.wallet.fixtures.UtxoDAOFixture
import org.bitcoins.wallet.Wallet
import org.bitcoins.testkit.wallet.WalletTestUtil
import org.bitcoins.testkit.wallet.BitcoinSWalletTest

class UTXOSpendingInfoDAOTest extends BitcoinSWalletTest with UtxoDAOFixture {
  behavior of "UTXOSpendingInfoDAO"

  it should "insert a segwit UTXO and read it" in { utxoDAO =>
    val utxo = WalletTestUtil.sampleSegwitUtxo

    for {
      created <- utxoDAO.create(utxo)
      read <- utxoDAO.read(created.id.get)
    } yield assert(read.contains(created))
  }

  it should "insert a legacy UTXO and read it" in {
    utxoDAO: UTXOSpendingInfoDAO =>
      val utxo = WalletTestUtil.sampleLegacyUtxo
      for {
        created <- utxoDAO.create(utxo)
        read <- utxoDAO.read(created.id.get)
      } yield assert(read.contains(created))
  }

  it should "insert a nested segwit UTXO and read it" ignore { _ =>
    ???
  }
}
