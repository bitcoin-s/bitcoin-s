package org.bitcoins.wallet.models

import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.wallet.util.{BitcoinSWalletTest, WalletTestUtil}

class AccountDAOTest extends BitcoinSWalletTest {

  it should "insert and read an account into the database" in {
    for {
      created <- {
        val account = WalletTestUtil.firstAccount

        val xpub = CryptoGenerators.extPublicKey.sample.get

        val accountDb = AccountDb(xpub, account)
        accountDAO.create(accountDb)
      }
      found <- accountDAO.read(
        (created.bip44Account.coin, created.bip44Account.index))
    } yield assert(found.contains(created))
  }
}
