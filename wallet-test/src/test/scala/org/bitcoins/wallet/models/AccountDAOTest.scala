package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.AccountDb
import org.bitcoins.testkitcore.Implicits._
import org.bitcoins.testkitcore.gen.CryptoGenerators
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.wallet.WalletTestUtil

class AccountDAOTest extends WalletDAOFixture {

  it should "insert and read an account into the database" in { daos =>
    val accountDAO = daos.accountDAO
    for {
      created <- {
        val account = WalletTestUtil.defaultHdAccount

        val xpub = CryptoGenerators.extPublicKey.sampleSome

        val accountDb = AccountDb(xpub, account)
        accountDAO.create(accountDb)
      }
      found <-
        accountDAO.read((created.hdAccount.coin, created.hdAccount.index))
    } yield assert(found.contains(created))
  }

  it must "find an account by HdAccount" in { daos =>
    val accountDAO = daos.accountDAO
    val account =
      WalletTestUtil.getHdAccount1(walletAppConfig = config)
    for {
      created <- {

        val xpub = CryptoGenerators.extPublicKey.sampleSome

        val accountDb =
          AccountDb(xpub, account)
        accountDAO.create(accountDb)
      }
      found <- accountDAO.findByAccount(account)
    } yield assert(found.contains(created))

  }
}
