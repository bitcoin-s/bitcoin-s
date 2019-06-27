package org.bitcoins.wallet.models

import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.wallet.Wallet
import org.bitcoins.testkit.wallet.WalletTestUtil
import org.bitcoins.testkit.wallet.BitcoinSWalletTest

class SpendingInfoDAOTest extends BitcoinSWalletTest with WalletDAOFixture {
  behavior of "SpendingInfoDAO"

  it should "insert a segwit UTXO and read it" in { daos =>
    val utxoDAO = daos.utxoDAO

    for {
      created <- utxoDAO.create(WalletTestUtil.sampleSegwitUTXO)
      read <- utxoDAO.read(created.id.get)
    } yield assert(read.contains(created))
  }

  it should "insert a legacy UTXO and read it" in { daos =>
    val utxoDAO = daos.utxoDAO
    for {
      created <- utxoDAO.create(WalletTestUtil.sampleLegacyUTXO)
      read <- utxoDAO.read(created.id.get)
    } yield assert(read.contains(created))
  }

  it should "insert a nested segwit UTXO and read it" ignore { _ =>
    ???
  }
}
