package org.bitcoins.wallet.models

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.wallet.fixtures.UtxoDAOFixture
import org.bitcoins.wallet.util.{BitcoinSWalletTest, WalletTestUtil}

class UTXOSpendingInfoDAOTest extends BitcoinSWalletTest with UtxoDAOFixture {
  behavior of "UTXOSpendingInfoDAO"

  it should "insert a UTXO and read it" in { utxoDAO =>
    val outpoint =
      TransactionOutPoint(WalletTestUtil.sampleTxid, WalletTestUtil.sampleVout)
    val output = TransactionOutput(Bitcoins.one, WalletTestUtil.sampleSPK)
    val privkeyPath = WalletTestUtil.sampleBip44Path
    val utxo =
      UTXOSpendingInfoDb(id = None,
                         outPoint = outpoint,
                         output = output,
                         privKeyPath = privkeyPath,
                         redeemScriptOpt = None, // todo test this properly
                         scriptWitnessOpt = None) // todo test this properly

    for {
      created <- utxoDAO.create(utxo)
      read <- utxoDAO.read(created.id.get)
    } yield assert(read.contains(created))
  }
}
