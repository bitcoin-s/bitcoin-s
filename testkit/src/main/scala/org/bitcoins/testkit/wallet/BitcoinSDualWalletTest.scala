package org.bitcoins.testkit.wallet

import org.bitcoins.commons.config.AppConfig
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc.models.ContractOraclePair
import org.bitcoins.dlc.wallet.{DLCAppConfig, DLCWallet}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.core.protocol.dlc.models.SingleContractInfo
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.wallet.DLCWalletUtil.InitializedDLCWallet
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import org.bitcoins.wallet.config.WalletAppConfig
import org.scalatest.FutureOutcome

import scala.concurrent.Future

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
    AppConfig.throwIfDefaultDatadir(getFreshConfig.walletConf)
    AppConfig.throwIfDefaultDatadir(config2.walletConf)
    AppConfig.throwIfDefaultDatadir(getFreshConfig.dlcConf)
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
          _ <- destroyDLCWallet(fundedWallets._1.wallet)
          _ <- destroyDLCWallet(fundedWallets._2.wallet)
        } yield ()
      }
    )(test)
  }

  /** Dual funded DLC wallets that are backed by a bitcoind node */
  def withDualFundedDLCWallets(
      test: OneArgAsyncTest,
      bitcoind: BitcoindRpcClient): FutureOutcome = {
    makeDependentFixture(
      build = () => {
        createDualFundedDLCWallet(nodeApi = bitcoind, chainQueryApi = bitcoind)
      },
      destroy = { fundedWallets: (FundedDLCWallet, FundedDLCWallet) =>
        destroyDLCWallets(dlcWallet1 = fundedWallets._1.wallet,
                          dlcWallet2 = fundedWallets._2.wallet)
      }
    )(test)
  }

  private def createDualFundedDLCWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi): Future[
    (FundedDLCWallet, FundedDLCWallet)] = {
    val walletAF = FundWalletUtil.createFundedDLCWallet(
      nodeApi = nodeApi,
      chainQueryApi = chainQueryApi,
      bip39PasswordOpt = getBIP39PasswordOpt(),
      extraConfig = Some(segwitWalletConf))
    val walletBF = FundWalletUtil.createFundedDLCWallet(
      nodeApi,
      chainQueryApi,
      getBIP39PasswordOpt(),
      Some(segwitWalletConf))(config2, system)
    for {
      walletA <- walletAF
      walletB <- walletBF
    } yield (walletA, walletB)
  }

  private def destroyDLCWallets(
      dlcWallet1: DLCWallet,
      dlcWallet2: DLCWallet): Future[Unit] = {
    val destroy1F = destroyDLCWallet(dlcWallet1)
    val destroy2F = destroyDLCWallet(dlcWallet2)
    for {
      _ <- destroy1F
      _ <- destroy2F
    } yield ()
  }

  /** Creates 2 funded segwit wallets that have a DLC initiated */
  def withDualDLCWallets(
      test: OneArgAsyncTest,
      contractOraclePair: ContractOraclePair): FutureOutcome = {
    makeDependentFixture(
      build = () => {
        createDualWalletsWithDLC(contractOraclePair = contractOraclePair,
                                 nodeApi = nodeApi,
                                 chainQueryApi = chainQueryApi)
      },
      destroy = { dlcWallets: (InitializedDLCWallet, InitializedDLCWallet) =>
        destroyDLCWallets(dlcWallet1 = dlcWallets._1.wallet,
                          dlcWallet2 = dlcWallets._2.wallet)
      }
    )(test)
  }

  def withDualDLCWallets(
      test: OneArgAsyncTest,
      contractOraclePair: ContractOraclePair,
      bitcoind: BitcoindRpcClient): FutureOutcome = {
    makeDependentFixture(
      build = () => {
        createDualWalletsWithDLC(contractOraclePair = contractOraclePair,
                                 bitcoind = bitcoind)
      },
      destroy = { dlcWallets: (InitializedDLCWallet, InitializedDLCWallet) =>
        destroyDLCWallets(dlcWallet1 = dlcWallets._1.wallet,
                          dlcWallet2 = dlcWallets._2.wallet)
      }
    )(test)
  }

  private def createDualWalletsWithDLC(
      contractOraclePair: ContractOraclePair,
      bitcoind: BitcoindRpcClient): Future[
    (InitializedDLCWallet, InitializedDLCWallet)] = {
    for {
      walletA <- FundWalletUtil.createFundedDLCWalletWithBitcoind(
        bitcoind,
        getBIP39PasswordOpt(),
        Some(segwitWalletConf))
      walletB <- FundWalletUtil.createFundedDLCWalletWithBitcoind(
        bitcoind = bitcoind,
        bip39PasswordOpt = getBIP39PasswordOpt(),
        extraConfig = Some(segwitWalletConf))(config2, system)
      amt = expectedDefaultAmt / Satoshis(2)
      contractInfo = SingleContractInfo(amt.satoshis, contractOraclePair)
      (dlcWalletA, dlcWalletB) <-
        DLCWalletUtil.initDLC(walletA, walletB, contractInfo)
    } yield (dlcWalletA, dlcWalletB)
  }

  private def createDualWalletsWithDLC(
      contractOraclePair: ContractOraclePair,
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi): Future[
    (InitializedDLCWallet, InitializedDLCWallet)] = {
    for {
      walletA <- FundWalletUtil.createFundedDLCWallet(
        nodeApi = nodeApi,
        chainQueryApi = chainQueryApi,
        bip39PasswordOpt = getBIP39PasswordOpt(),
        extraConfig = Some(segwitWalletConf))
      walletB <- FundWalletUtil.createFundedDLCWallet(
        nodeApi = nodeApi,
        chainQueryApi = chainQueryApi,
        bip39PasswordOpt = getBIP39PasswordOpt(),
        extraConfig = Some(segwitWalletConf))(config2, system)
      amt = expectedDefaultAmt / Satoshis(2)
      contractInfo = SingleContractInfo(amt.satoshis, contractOraclePair)
      (dlcWalletA, dlcWalletB) <-
        DLCWalletUtil.initDLC(walletA, walletB, contractInfo)
    } yield (dlcWalletA, dlcWalletB)
  }
}
