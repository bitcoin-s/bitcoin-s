package org.bitcoins.wallet

import org.bitcoins.core.api.wallet.WalletApi
import org.bitcoins.core.currency._
import org.bitcoins.testkitcore.Implicits._
import org.bitcoins.testkitcore.gen.TransactionGenerators
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest.FutureOutcome
import org.scalatest.compatible.Assertion

import scala.concurrent.Future

class ProcessTransactionTest extends BitcoinSWalletTest {
  override type FixtureParam = WalletApi

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withNewWallet(test, getBIP39PasswordOpt())
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
      assert(oldUtxos == newUtxos)
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
}
