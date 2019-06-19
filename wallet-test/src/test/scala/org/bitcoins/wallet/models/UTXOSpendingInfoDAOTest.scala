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

class UTXOSpendingInfoDAOTest extends BitcoinSWalletTest with WalletDAOFixture {
  behavior of "UTXOSpendingInfoDAO"

  it should "insert a segwit UTXO and read it" in { daos =>
    val utxoDAO = daos.utxoDAO

    for {
      (tx, _) <- WalletTestUtil.insertIncomingTx(daos)
      created <- {
        val utxo = {
          val outpoint =
            TransactionOutPoint(WalletTestUtil.sampleTxid,
                                WalletTestUtil.sampleVout)
          val output = TransactionOutput(1.bitcoin, WalletTestUtil.sampleSPK)
          val scriptWitness = WalletTestUtil.sampleScriptWitness
          val privkeyPath = WalletTestUtil.sampleSegwitPath
          NativeV0UTXOSpendingInfoDb(outPoint = outpoint,
                                     output = output,
                                     privKeyPath = privkeyPath,
                                     scriptWitness = scriptWitness,
                                     incomingTxId = tx.id.get)
        }
        utxoDAO.create(utxo)
      }
      read <- utxoDAO.read(created.id.get)
    } yield assert(read.contains(created))
  }

  it should "insert a legacy UTXO and read it" in { daos =>
    val utxoDAO = daos.utxoDAO
    for {
      (tx, _) <- WalletTestUtil.insertIncomingTx(daos)
      created <- {
        val utxo = {
          val outpoint =
            TransactionOutPoint(WalletTestUtil.sampleTxid,
                                WalletTestUtil.sampleVout)
          val output = TransactionOutput(1.bitcoin, WalletTestUtil.sampleSPK)
          val privKeyPath = WalletTestUtil.sampleLegacyPath
          LegacyUTXOSpendingInfoDb(outPoint = outpoint,
                                   output = output,
                                   privKeyPath = privKeyPath,
                                   incomingTxId = tx.id.get)
        }
        utxoDAO.create(utxo)
      }
      read <- utxoDAO.read(created.id.get)
    } yield assert(read.contains(created))
  }

  it should "insert a nested segwit UTXO and read it" ignore { _ =>
    ???
  }
}
