package org.bitcoins.wallet

import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.NodeUnitTest.NeutrinoNodeFundedWalletBitcoind
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.BitcoinSWalletTest.WalletWithBitcoind
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
    withFundedWalletAndBitcoind(test)
  }

  behavior of "Wallet rescans"


  private def clearSpendingInfoTable(wallet: WalletApi): Future[Int] = {
    import slick.jdbc.SQLiteProfile.api._

    val conf: WalletAppConfig = wallet.walletConfig
    val table = TableQuery[SpendingInfoTable]
    conf.database.run(table.delete)
  }

  it must "be able to discover funds that belong to the wallet using WalletApi.rescanNeutrinoWallet" in { fixture: WalletWithBitcoind =>

    val WalletWithBitcoind(wallet, bitcoind) = fixture

    val initBalanceF = wallet.getBalance()
    val bestHashHeightF = wallet.chainQueryApi.getBestHashBlockHeight()

    val balanceClearedF = for {
      _ <- initBalanceF
      _ <- clearSpendingInfoTable(wallet)
      b <- wallet.getBalance()
    } yield b

    val rescanF = for {
      initBalance <- initBalanceF
      bestHashHeight <- bestHashHeightF
      clearedBalance <- balanceClearedF
      _ = assert(clearedBalance == CurrencyUnits.zero, s"Balance after clearing wallet should be zero! Got=$clearedBalance")
      tipOpt = Some(BlockStamp.BlockHeight(bestHashHeight))
      _ <- wallet.rescanNeutrinoWallet(BlockStamp.height0Opt, tipOpt, 100)
      balanceAfterRescan <- wallet.getBalance()
    } yield  {
      assert(balanceAfterRescan == initBalance)
    }

    rescanF
  }
}
