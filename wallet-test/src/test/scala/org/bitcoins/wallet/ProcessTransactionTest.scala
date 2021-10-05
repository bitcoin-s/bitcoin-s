package org.bitcoins.wallet

import org.bitcoins.core.api.wallet.WalletApi
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}
import org.bitcoins.testkitcore.Implicits._
import org.bitcoins.testkitcore.gen.TransactionGenerators
import org.scalatest.FutureOutcome
import org.scalatest.compatible.Assertion

import scala.concurrent.Future

class ProcessTransactionTest extends BitcoinSWalletTest {
  override type FixtureParam = Wallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withNewWallet(test, getBIP39PasswordOpt())(getFreshWalletAppConfig)
  }

  behavior of "Wallet.processTransaction"

  /** Verifies that executing the given action doesn't change wallet state */
  private def checkUtxosAndBalance(wallet: WalletApi)(
      action: => Future[_]): Future[Assertion] =
    for {
      oldTransactions <- wallet.listTransactions()
      oldUtxos <- wallet.listUtxos()
      oldUnconfirmed <- wallet.getUnconfirmedBalance()
      oldConfirmed <- wallet.getBalance()
      _ <- action // by name
      newTransactions <- wallet.listTransactions()
      newUtxos <- wallet.listUtxos()
      newUnconfirmed <- wallet.getUnconfirmedBalance()
      newConfirmed <- wallet.getBalance()

    } yield {
      assert(oldConfirmed == newConfirmed)
      assert(oldUnconfirmed == newUnconfirmed)
      // make utxos comparable
      val comparableOldUtxos =
        oldUtxos.map(_.copyWithId(0)).sortBy(_.outPoint.hex)
      val comparableNewUtxos =
        newUtxos.map(_.copyWithId(0)).sortBy(_.outPoint.hex)
      assert(comparableOldUtxos == comparableNewUtxos)
      assert(oldTransactions == newTransactions)
    }

  it must "change state when processing a transaction with a block hash" in {
    wallet =>
      for {
        address <- wallet.getNewAddress()
        tx =
          TransactionGenerators
            .transactionTo(address.scriptPubKey)
            .sampleSome

        _ <- wallet.processTransaction(tx, None)
        oldConfirmed <- wallet.getConfirmedBalance()
        oldUnconfirmed <- wallet.getUnconfirmedBalance()

        // repeating the action should not make a difference
        _ <- checkUtxosAndBalance(wallet) {
          wallet.processTransaction(tx, None)
        }

        _ <- wallet.processTransaction(tx, Some(testBlockHash))
        newConfirmed <- wallet.getConfirmedBalance()
        newUnconfirmed <- wallet.getUnconfirmedBalance()
        utxosPostAdd <- wallet.listUtxos()

        // repeating the action should not make a difference
        _ <- checkUtxosAndBalance(wallet) {
          wallet.processTransaction(tx, Some(testBlockHash))
        }
      } yield {
        val ourOutputs =
          tx.outputs.filter(_.scriptPubKey == address.scriptPubKey)

        assert(utxosPostAdd.length == ourOutputs.length)
        assert(newConfirmed != oldConfirmed)
        assert(newUnconfirmed != oldUnconfirmed)
      }
  }

  it must "not change state when processing the same transaction twice" in {
    wallet =>
      for {
        address <- wallet.getNewAddress()
        tx =
          TransactionGenerators
            .transactionTo(address.scriptPubKey)
            .sampleSome

        _ <- wallet.processTransaction(tx, None)
        oldConfirmed <- wallet.getConfirmedBalance()
        oldUnconfirmed <- wallet.getUnconfirmedBalance()

        // repeating the action should not make a difference
        _ <- checkUtxosAndBalance(wallet) {
          wallet.processTransaction(tx, None)
        }

        _ <- wallet.processTransaction(tx, None)
        newConfirmed <- wallet.getConfirmedBalance()
        newUnconfirmed <- wallet.getUnconfirmedBalance()
        utxosPostAdd <- wallet.listUtxos()

        // repeating the action should not make a difference
        _ <- checkUtxosAndBalance(wallet) {
          wallet.processTransaction(tx, None)
        }
      } yield {
        val ourOutputs =
          tx.outputs.filter(_.scriptPubKey == address.scriptPubKey)

        assert(utxosPostAdd.length == ourOutputs.length)
        assert(newConfirmed == oldConfirmed)
        assert(newUnconfirmed == oldUnconfirmed)
      }
  }

  it must "not change state when processing an unrelated transaction" in {
    wallet =>
      val unrelated = TransactionGenerators.transaction.sampleSome
      for {
        _ <- checkUtxosAndBalance(wallet) {
          wallet.processTransaction(unrelated, None)
        }

        balance <- wallet.getBalance()
        unconfirmed <- wallet.getUnconfirmedBalance()

      } yield {
        assert(balance == 0.sats)
        assert(unconfirmed == 0.sats)
      }
  }

  it must "spend and receive funds in the same transaction where the funding utxo is reserved" in {
    wallet =>
      val fundingAddressF = wallet.getNewAddress()
      val receivingAddressF = wallet.getNewAddress()
      val amount = Bitcoins.one

      val amtWithFee = amount + Satoshis(175) //for fee

      //build funding tx
      val fundingTxF: Future[(Transaction, UInt32)] = for {
        fundingAddr <- fundingAddressF
        fundingTx = TransactionGenerators.buildCreditingTransaction(
          fundingAddr.scriptPubKey,
          amtWithFee)
      } yield fundingTx

      val processedFundingTxF: Future[WalletApi] = for {
        (fundingTx, _) <- fundingTxF
        //make sure wallet is empty
        balance <- wallet.getBalance()
        _ = assert(balance == Bitcoins.zero)
        processed <- wallet.processTransaction(fundingTx, None)
        balance <- wallet.getBalance()
        _ = assert(balance == amtWithFee)
      } yield processed

      //build spending tx
      val spendingTxF = for {
        receivingAddress <- receivingAddressF
        wallet <- processedFundingTxF
        destinations = Vector(
          TransactionOutput(amount, receivingAddress.scriptPubKey))
        spendingTx <- wallet.fundRawTransaction(
          destinations = destinations,
          feeRate = SatoshisPerByte.one,
          fromTagOpt = None,
          markAsReserved = true
        )
        processedSpendingTx <- wallet.processTransaction(transaction =
                                                           spendingTx,
                                                         blockHash = None)
        balance <- processedSpendingTx.getBalance()
      } yield assert(balance == amount)

      spendingTxF
  }

  it must "get the unconfirmed balance of an account" in { wallet: Wallet =>
    val account1 = WalletTestUtil.getHdAccount1(wallet.walletConfig)
    for {
      address <- wallet.getNewAddress()
      tx =
        TransactionGenerators
          .transactionTo(address.scriptPubKey)
          .sampleSome

      _ <- wallet.processTransaction(tx, None)
      accountBalance <- wallet.getUnconfirmedBalance(account1)
    } yield {
      assert(accountBalance == CurrencyUnits.zero)
    }
  }
}
