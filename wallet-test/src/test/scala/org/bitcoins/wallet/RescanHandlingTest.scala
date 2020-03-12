package org.bitcoins.wallet

import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.NodeUnitTest.NeutrinoNodeFundedWalletBitcoind
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.BitcoinSWalletTest.{WalletWithBitcoind, WalletWithBitcoindV19}
import org.bitcoins.wallet.api.WalletApi
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.SpendingInfoTable
import org.scalatest.FutureOutcome

import scala.concurrent.Future

class RescanHandlingTest extends BitcoinSWalletTest {

  /** Wallet config with data directory set to user temp directory */
  implicit override protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoTestConfig()

  override type FixtureParam = WalletWithBitcoind
  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFundedWalletAndBitcoindV19(test)
  }

  behavior of "Wallet rescans"

  it must "be able to discover funds that belong to the wallet using WalletApi.rescanNeutrinoWallet" in { fixture: WalletWithBitcoind =>

    val WalletWithBitcoindV19(wallet, bitcoind) = fixture

    val initBalanceF = wallet.getBalance()

    val rescanF = for {
      initBalance <- initBalanceF
      _ = assert(initBalance > CurrencyUnits.zero, s"Cannot run rescan test if our init wallet balance is zero!")
      _ <- wallet.fullRescanNeurinoWallet(10)
      balanceAfterRescan <- wallet.getBalance()
    } yield  {
      assert(balanceAfterRescan == initBalance)
    }

    rescanF
  }

  it must "be able to discover funds that occurred in a certain range of block hashes"

  it must "NOT discover funds that happened OUTSIDE of a certain range of block hashes"


}
