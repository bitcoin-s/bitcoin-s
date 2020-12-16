package org.bitcoins.wallet

import com.typesafe.config.ConfigFactory
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.keymanager.KeyManagerTestUtil.bip39PasswordOpt
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.BitcoinSWalletTest._
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}

class MultiWalletTest extends BitcoinSAsyncTest with EmbeddedPg {

  it must "create 2 different wallets" in {
    val walletNameConfA =
      ConfigFactory.parseString(s"bitcoin-s.wallet.walletName = walletA")
    val walletNameConfB =
      ConfigFactory.parseString(s"bitcoin-s.wallet.walletName = walletB")

    val dbConf =
      BitcoinSTestAppConfig.configWithEmbeddedDb(project = None, () => pgUrl())

    val dir = BitcoinSTestAppConfig.tmpDir()

    val configA = BitcoinSAppConfig(dir, walletNameConfA.withFallback(dbConf))
    val configB = BitcoinSAppConfig(dir, walletNameConfB.withFallback(dbConf))

    for {
      walletA <- BitcoinSWalletTest.createDefaultWallet(
        MockNodeApi,
        MockChainQueryApi,
        bip39PasswordOpt)(configA, system.dispatcher)
      accountA <- walletA.getDefaultAccount()

      walletB <- BitcoinSWalletTest.createDefaultWallet(
        MockNodeApi,
        MockChainQueryApi,
        bip39PasswordOpt)(configB, system.dispatcher)
      accountB <- walletB.getDefaultAccount()
    } yield assert(accountA.xpub != accountB.xpub)
  }
}
