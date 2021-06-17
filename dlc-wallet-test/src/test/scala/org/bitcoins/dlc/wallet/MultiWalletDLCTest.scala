package org.bitcoins.dlc.wallet

import com.typesafe.config.ConfigFactory
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.keymanager.KeyManagerTestUtil.bip39PasswordOpt
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.BitcoinSWalletTest._
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import org.scalatest.FutureOutcome

class MultiWalletDLCTest extends BitcoinSWalletTest {

  override type FixtureParam = FundedDLCWallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withFundedDLCWallet(test, getBIP39PasswordOpt())(getFreshConfig)

  it must "create 2 different dlc wallets" in { fundedWallet =>
    val walletNameConfB =
      ConfigFactory.parseString(s"bitcoin-s.wallet.walletName = walletB")

    val dbConf =
      BitcoinSTestAppConfig.configWithEmbeddedDb(project = None, () => pgUrl())

    val dir = BitcoinSTestAppConfig.tmpDir()

    val configB = BitcoinSAppConfig(dir, walletNameConfB.withFallback(dbConf))

    val walletA = fundedWallet.wallet

    val walletBF = BitcoinSWalletTest.createDLCWallet2Accounts(
      MockNodeApi,
      MockChainQueryApi,
      bip39PasswordOpt)(configB, system)

    for {
      accountA <- walletA.getDefaultAccount()

      walletB <- walletBF
      accountB <- walletB.getDefaultAccount()

      _ = assert(accountA.xpub != accountB.xpub)

      _ <- walletA.createDLCOffer(sampleContractInfo,
                                  half,
                                  Some(SatoshisPerVirtualByte.one),
                                  UInt32.zero,
                                  UInt32.one)
      dlcsA <- walletA.listDLCs()
      dlcsB <- walletB.listDLCs()

      // only stop walletB, walletA will be stopped by the fixture
      _ <- destroyDLCWallet(walletB)
    } yield {
      assert(dlcsA.size == 1)
      assert(dlcsB.isEmpty)
      assert(dlcsA != dlcsB)
    }
  }
}
