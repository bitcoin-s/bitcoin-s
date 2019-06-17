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
    val outpoint =
      TransactionOutPoint(WalletTestUtil.sampleTxid, WalletTestUtil.sampleVout)
    val output = TransactionOutput(1.bitcoin, WalletTestUtil.sampleSPK)
    val scriptWitness = WalletTestUtil.sampleScriptWitness
    val privkeyPath = WalletTestUtil.sampleSegwitPath
    val utxo =
      NativeV0UTXOSpendingInfoDb(
        id = None,
        outPoint = outpoint,
        output = output,
        privKeyPath = privkeyPath,
        scriptWitness = scriptWitness) // todo test this properly

    for {
      created <- utxoDAO.create(utxo)
      read <- utxoDAO.read(created.id.get)
    } yield assert(read.contains(created))
  }

  it should "insert a legacy UTXO and read it" in { utxoDAO =>
    val outpoint =
      TransactionOutPoint(WalletTestUtil.sampleTxid, WalletTestUtil.sampleVout)
    val output = TransactionOutput(1.bitcoin, WalletTestUtil.sampleSPK)
    val privKeyPath = WalletTestUtil.sampleLegacyPath
    val utxo = LegacyUTXOSpendingInfoDb(id = None,
                                        outPoint = outpoint,
                                        output = output,
                                        privKeyPath = privKeyPath)
    for {
      created <- utxoDAO.create(utxo)
      read <- utxoDAO.read(created.id.get)
    } yield assert(read.contains(created))
  }

  it should "insert a nested segwit UTXO and read it" ignore { _ =>
    ???
  }
}
