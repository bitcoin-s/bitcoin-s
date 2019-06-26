package org.bitcoins.wallet.models

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.core.gen.TransactionGenerators
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.wallet.WalletTestUtil
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.wallet.config.WalletAppConfig
import org.bouncycastle.crypto.tls.CertChainType
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.core.hd.LegacyHDPath
import scala.concurrent.Future
import org.bitcoins.testkit.fixtures.WalletDAOs

class IncomingTxoDAOTest extends BitcoinSWalletTest with WalletDAOFixture {

  it must "insert a incoming transaction and read it back with its address" in {
    daos =>
      val WalletDAOs(_, _, txoDAO, _, utxoDAO) = daos

      for {
        utxo <- utxoDAO.create(WalletTestUtil.sampleLegacyUTXO)
        (txo, _) <- WalletTestUtil.insertIncomingTxo(daos, utxo)
        foundTxos <- txoDAO.findTx(txo.txid)
      } yield assert(foundTxos.contains(txo))
  }
}
