package org.bitcoins.server

import akka.stream.KillSwitches
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.server.util.CallbackUtil
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.bitcoins.testkitcore.Implicits.GeneratorOps
import org.bitcoins.testkitcore.gen.TransactionGenerators
import org.scalatest.FutureOutcome

import scala.concurrent.duration.DurationInt

class CallBackUtilTest extends BitcoinSWalletTest {

  behavior of "CallBackUtil"

  override type FixtureParam = FundedWallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withFundedWallet(test, getBIP39PasswordOpt())(getFreshWalletAppConfig)

  it must "have the kill switch kill messages to the createBitcoindNodeCallbacksForWallet callback" in {
    fundedWallet =>
      val wallet = fundedWallet.wallet
      val addressF = wallet.getNewAddress()
      val initBalanceF = wallet.getBalance()
      val tx1F = addressF.map { addr =>
        TransactionGenerators
          .transactionTo(addr.scriptPubKey)
          .sampleSome
      }
      val tx2F = addressF.map { addr =>
        TransactionGenerators
          .transactionTo(addr.scriptPubKey)
          .sampleSome
      }

      val killSwitch = KillSwitches.shared("callbackutil-test-killswitch")
      val callbacksF =
        CallbackUtil.createBitcoindNodeCallbacksForWallet(wallet, killSwitch)
      for {
        tx1 <- tx1F
        tx2 <- tx2F
        callbacks <- callbacksF
        _ <- callbacks.executeOnTxReceivedCallbacks(logger, tx1)
        _ <- AsyncUtil.nonBlockingSleep(1000.millis)
        initBalance <- initBalanceF
        balance2 <- wallet.getBalance()
        _ = killSwitch.shutdown()
        _ <- callbacks.executeOnTxReceivedCallbacks(logger, tx2)
        _ <- AsyncUtil.nonBlockingSleep(1000.millis)
        balance3 <- wallet.getBalance()
        _ <- callbacks.stop()
      } yield {
        assert(balance2 > initBalance)
        assert(balance3 == balance2)
      }
  }

  it must "have the kill switch kill messages to the createNeutrinoNodeCallbacksForWallet callback" in {
    fundedWallet =>
      val wallet = fundedWallet.wallet
      val addressF = wallet.getNewAddress()
      val initBalanceF = wallet.getBalance()
      val tx1F = addressF.map { addr =>
        TransactionGenerators
          .transactionTo(addr.scriptPubKey)
          .sampleSome
      }
      val tx2F = addressF.map { addr =>
        TransactionGenerators
          .transactionTo(addr.scriptPubKey)
          .sampleSome
      }

      val killSwitch = KillSwitches.shared("callbackutil-test2-killswitch")
      val callbacksF =
        CallbackUtil.createNeutrinoNodeCallbacksForWallet(wallet, killSwitch)
      for {
        tx1 <- tx1F
        tx2 <- tx2F
        callbacks <- callbacksF
        _ <- callbacks.executeOnTxReceivedCallbacks(logger, tx1)
        _ <- AsyncUtil.nonBlockingSleep(1000.millis)
        initBalance <- initBalanceF
        balance2 <- wallet.getBalance()
        _ = killSwitch.shutdown()
        _ <- callbacks.executeOnTxReceivedCallbacks(logger, tx2)
        _ <- AsyncUtil.nonBlockingSleep(1000.millis)
        balance3 <- wallet.getBalance()
        _ <- callbacks.stop()
      } yield {
        assert(balance2 > initBalance)
        assert(balance3 == balance2)
      }
  }
}
