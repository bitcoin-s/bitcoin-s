package org.bitcoins.server

import akka.stream.StreamDetachedException
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.server.util.CallbackUtil
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import org.bitcoins.testkitcore.Implicits.GeneratorOps
import org.bitcoins.testkitcore.gen.TransactionGenerators
import org.scalatest.FutureOutcome

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class CallBackUtilTest extends BitcoinSWalletTest {

  behavior of "CallBackUtil"

  override type FixtureParam = FundedDLCWallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withFundedDLCWallet(test)(getFreshConfig)

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

      val callbacksF =
        CallbackUtil.createBitcoindNodeCallbacksForWallet(wallet)
      for {
        tx1 <- tx1F
        tx2 <- tx2F
        callbacks <- callbacksF
        initBalance <- initBalanceF
        _ <- callbacks.executeOnTxReceivedCallbacks(logger, tx1)
        _ <- AsyncUtil.nonBlockingSleep(5000.millis)
        balance2 <- wallet.getBalance()
        _ <- callbacks.stop()
        _ <- callbacks
          .executeOnTxReceivedCallbacks(logger, tx2)
          .recoverWith { case _: StreamDetachedException =>
            //expect the stream to be detatched because we stopped
            //the stream with callbacks.stop()
            Future.unit
          }
        balance3 <- wallet.getBalance()
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

      val callbacksF =
        CallbackUtil.createNeutrinoNodeCallbacksForWallet(wallet)
      for {
        tx1 <- tx1F
        tx2 <- tx2F
        callbacks <- callbacksF
        initBalance <- initBalanceF
        _ <- callbacks.executeOnTxReceivedCallbacks(logger, tx1)
        _ <- AsyncUtil.nonBlockingSleep(5000.millis)
        balance2 <- wallet.getBalance()
        _ <- callbacks.stop()
        _ <- callbacks
          .executeOnTxReceivedCallbacks(logger, tx2)
          .recoverWith { case _: StreamDetachedException =>
            //expect the stream to be detatched because we stopped
            //the stream with callbacks.stop()
            Future.unit
          }
        balance3 <- wallet.getBalance()
      } yield {
        assert(balance2 > initBalance)
        assert(balance3 == balance2)
      }
  }

  override def afterAll(): Unit = {
    super[BitcoinSWalletTest].afterAll()
  }
}
