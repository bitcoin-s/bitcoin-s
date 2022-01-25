package org.bitcoins.dlc.wallet

import com.typesafe.config.ConfigFactory
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.TxoState
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

    val configB =
      BitcoinSAppConfig(dir, Vector(walletNameConfB.withFallback(dbConf)))

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

  it must "create an offer, out of band unreserve the utxo, and then cancel the offer" in {
    fundedWallet: FundedDLCWallet =>
      //see: https://github.com/bitcoin-s/bitcoin-s/issues/3813#issue-1051117559
      val wallet = fundedWallet.wallet
      val offerF = wallet.createDLCOffer(contractInfo = sampleContractInfo,
                                         collateral = half,
                                         feeRateOpt =
                                           Some(SatoshisPerVirtualByte.one),
                                         locktime = UInt32.zero,
                                         refundLocktime = UInt32.one)

      //now unreserve the utxo
      val reservedUtxoF = for {
        _ <- offerF
        utxos <- wallet.listUtxos(TxoState.Reserved)
        _ <- wallet.unmarkUTXOsAsReserved(utxos)
      } yield ()

      //now cancel the offer
      for {
        offer <- offerF
        _ <- reservedUtxoF
        _ <- wallet.cancelDLC(offer.dlcId)
      } yield succeed
  }
}
