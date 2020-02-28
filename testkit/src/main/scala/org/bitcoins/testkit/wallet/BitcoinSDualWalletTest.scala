package org.bitcoins.testkit.wallet

import org.bitcoins.db.AppConfig
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.wallet.DLCWalletUtil.InitializedDLCWallet
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.bitcoins.wallet.config.WalletAppConfig
import org.scalatest.FutureOutcome

trait BitcoinSDualWalletTest extends BitcoinSWalletTest {
  import BitcoinSWalletTest._

  implicit protected def config2: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig()

  implicit protected def wallet2AppConfig: WalletAppConfig = {
    config2.walletConf
  }

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(config.walletConf)
    AppConfig.throwIfDefaultDatadir(config2.walletConf)
  }

  def withDualFundedSegwitWallets(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture(
      build = () =>
        for {
          walletA <- FundWalletUtil.createFundedWallet(nodeApi,
                                                       chainQueryApi,
                                                       Some(segwitWalletConf))
          walletB <- FundWalletUtil.createFundedWallet(
            nodeApi,
            chainQueryApi,
            Some(segwitWalletConf))(config2, system)
        } yield (walletA, walletB),
      destroy = { fundedWallets: (FundedWallet, FundedWallet) =>
        destroyWallet(fundedWallets._1.wallet)
        destroyWallet(fundedWallets._2.wallet)
      }
    )(test)
  }

  def withDualDLCWallets(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture(
      build = () =>
        for {
          walletA <- FundWalletUtil.createFundedWallet(nodeApi,
                                                       chainQueryApi,
                                                       Some(segwitWalletConf))
          walletB <- FundWalletUtil.createFundedWallet(
            nodeApi,
            chainQueryApi,
            Some(segwitWalletConf))(config2, system)
          (dlcWalletA, dlcWalletB) <- DLCWalletUtil.createDLCWallets(walletA,
                                                                     walletB)
        } yield (dlcWalletA, dlcWalletB),
      destroy = { dlcWallets: (InitializedDLCWallet, InitializedDLCWallet) =>
        destroyWallet(dlcWallets._1.wallet)
        destroyWallet(dlcWallets._2.wallet)
      }
    )(test)
  }
}
