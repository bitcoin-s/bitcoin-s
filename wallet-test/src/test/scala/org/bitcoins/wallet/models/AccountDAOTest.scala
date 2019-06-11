package org.bitcoins.wallet.models

import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.wallet.fixtures.AccountDAOFixture
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}

class AccountDAOTest extends BitcoinSWalletTest with AccountDAOFixture {

  it should "insert and read an account into the database" in { accountDAO =>
    for {
      created <- {
        val account = WalletTestUtil.firstAccount

        val xpub = CryptoGenerators.extPublicKey.sample.get

        val accountDb = AccountDb(xpub, account)
        accountDAO.create(accountDb)
      }
      found <- accountDAO.read(
        (created.hdAccount.coin, created.hdAccount.index))
    } yield assert(found.contains(created))
  }
}
