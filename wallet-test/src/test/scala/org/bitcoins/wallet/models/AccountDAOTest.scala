package org.bitcoins.wallet.models

import java.time.{ZoneId, ZonedDateTime}

import org.bitcoins.core.util.TimeUtil
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}
import org.bitcoins.testkit.Implicits._

class AccountDAOTest extends BitcoinSWalletTest with WalletDAOFixture {

  it should "insert and read an account into the database" in { daos =>
    val accountDAO = daos.accountDAO
    for {
      created <- {
        val account = WalletTestUtil.defaultHdAccount

        val xpub = CryptoGenerators.extPublicKey.sampleSome

        val accountDb =
          AccountDb(xpub, account, TimeUtil.currentEpochSecond)
        accountDAO.create(accountDb)
      }
      found <- accountDAO.read(
        (created.hdAccount.coin, created.hdAccount.index))
    } yield assert(found.contains(created))
  }

  it must "find an account by HdAccount" in { daos =>
    val accountDAO = daos.accountDAO
    val account =
      WalletTestUtil.getHdAccount1(walletAppConfig = walletAppConfig)
    for {
      created <- {

        val xpub = CryptoGenerators.extPublicKey.sampleSome

        val accountDb =
          AccountDb(xpub, account, TimeUtil.currentEpochSecond)
        accountDAO.create(accountDb)
      }
      found <- accountDAO.findByAccount(account)
    } yield assert(found.contains(created))

  }
}
