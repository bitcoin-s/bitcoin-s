package org.bitcoins.wallet.models

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.core.gen.TransactionGenerators
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.wallet.fixtures._
import org.bitcoins.testkit.wallet.WalletTestUtil
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.wallet.config.WalletAppConfig
import org.bouncycastle.crypto.tls.CertChainType
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.core.hd.LegacyHDPath

class IncomingTransactionDAOTest
    extends BitcoinSWalletTest
    with WalletDAOFixture {
  private def getTx: Transaction =
    TransactionGenerators.transaction
      .suchThat(_.outputs.nonEmpty)
      .sample
      .getOrElse(getTx)

  it must "insert a incoming transaction and read it back with its address" in {
    daos =>
      val txDao = daos.incomingTxDAO
      val addrDao = daos.addressDAO
      implicit val walletconf: WalletAppConfig = config
      val accountDAO = AccountDAO()

      val account = WalletTestUtil.firstAccountDb

      val address = {
        val pub = ECPublicKey()
        val path =
          account.hdAccount
            .toChain(HDChainType.External)
            .toAddress(0)
            .toPath

        AddressDbHelper.getAddress(pub, path, RegTest)
      }

      val tx = getTx
      val txDb = IncomingTransaction(tx,
                                     confirmations = 3,
                                     scriptPubKey = address.scriptPubKey)
      import org.bitcoins.db.DbCommonsColumnMappers._
      for {
        _ <- accountDAO.create(account)
        createdAddress <- addrDao.create(address)
        createdTx <- txDao.create(txDb)
        txAndAddr <- txDao.withAddress(createdTx.transaction)
      } yield {
        txAndAddr match {
          case None                       => fail(s"Couldn't read back TX with address from DB!")
          case Some((foundTx, foundAddr)) =>
            // can't do just foundTx == txDb, ID's are different (None/Some(_))
            assert(foundTx.confirmations == txDb.confirmations)
            assert(foundTx.scriptPubKey == txDb.scriptPubKey)
            assert(foundTx.transaction == txDb.transaction)

            assert(foundAddr == address)
        }
      }
  }
}
