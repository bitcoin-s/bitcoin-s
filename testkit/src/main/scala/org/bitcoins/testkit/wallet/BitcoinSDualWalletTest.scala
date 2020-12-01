package org.bitcoins.testkit.wallet

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.OracleAndContractInfo
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.db.AppConfig
import org.bitcoins.dlc.testgen.DLCTestUtil
import org.bitcoins.dlc.wallet.DLCAppConfig
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.wallet.DLCWalletUtil.InitializedDLCWallet
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import org.bitcoins.wallet.config.WalletAppConfig
import org.scalatest.FutureOutcome

trait BitcoinSDualWalletTest extends BitcoinSWalletTest {
  import BitcoinSWalletTest._

  implicit protected def config2: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig()

  implicit protected def wallet2AppConfig: WalletAppConfig = {
    config2.walletConf
  }

  implicit protected def dlc2AppConfig: DLCAppConfig = {
    config2.dlcConf
  }

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(config.walletConf)
    AppConfig.throwIfDefaultDatadir(config2.walletConf)
    AppConfig.throwIfDefaultDatadir(config.dlcConf)
    AppConfig.throwIfDefaultDatadir(config2.dlcConf)
    super.beforeAll()
  }

  /** Creates two segwit wallets that are funded with some bitcoin, these wallets are NOT
    * peered with a bitcoind so the funds in the wallets are not tied to an
    * underlying blockchain
    */
  def withDualFundedDLCWallets(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture(
      build = () =>
        for {
          walletA <-
            FundWalletUtil.createFundedDLCWallet(nodeApi,
                                                 chainQueryApi,
                                                 getBIP39PasswordOpt(),
                                                 Some(segwitWalletConf))
          walletB <- FundWalletUtil.createFundedDLCWallet(
            nodeApi,
            chainQueryApi,
            getBIP39PasswordOpt(),
            Some(segwitWalletConf))(config2, system)
        } yield (walletA, walletB),
      destroy = { fundedWallets: (FundedDLCWallet, FundedDLCWallet) =>
        for {
          _ <- destroyWallet(fundedWallets._1.wallet)
          _ <- destroyWallet(fundedWallets._2.wallet)
        } yield ()
      }
    )(test)
  }

  /** Creates 2 funded segwit wallets that have a DLC initiated */
  def withDualDLCWallets(
      test: OneArgAsyncTest,
      multiNonce: Boolean): FutureOutcome = {
    makeDependentFixture(
      build = () =>
        for {
          walletA <-
            FundWalletUtil.createFundedDLCWallet(nodeApi,
                                                 chainQueryApi,
                                                 getBIP39PasswordOpt(),
                                                 Some(segwitWalletConf))
          walletB <- FundWalletUtil.createFundedDLCWallet(
            nodeApi,
            chainQueryApi,
            getBIP39PasswordOpt(),
            Some(segwitWalletConf))(config2, system)

          oracleAndContractInfo =
            if (multiNonce) {
              DLCWalletUtil.multiNonceOracleAndContractInfo
            } else {
              val numOutcomes = 8
              val outcomes = DLCTestUtil.genOutcomes(numOutcomes)
              val (contractInfo, _) =
                DLCTestUtil.genContractInfos(outcomes, Satoshis(10000))

              OracleAndContractInfo(DLCWalletUtil.sampleOracleInfo,
                                    contractInfo)
            }

          (dlcWalletA, dlcWalletB) <-
            DLCWalletUtil.initDLC(walletA, walletB, oracleAndContractInfo)
        } yield (dlcWalletA, dlcWalletB),
      destroy = { dlcWallets: (InitializedDLCWallet, InitializedDLCWallet) =>
        for {
          _ <- destroyWallet(dlcWallets._1.wallet)
          _ <- destroyWallet(dlcWallets._2.wallet)
        } yield ()
      }
    )(test)
  }
}
