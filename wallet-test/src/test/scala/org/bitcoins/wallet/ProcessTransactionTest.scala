package org.bitcoins.wallet

import org.bitcoins.core.api.wallet.WalletApi
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  Transaction,
  TransactionConstants,
  TransactionInput,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkitcore.Implicits._
import org.bitcoins.testkitcore.gen.{CryptoGenerators, TransactionGenerators}
import org.scalatest.FutureOutcome
import org.scalatest.compatible.Assertion

import scala.concurrent.Future

class ProcessTransactionTest extends BitcoinSWalletTest {
  override type FixtureParam = WalletApi

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

  it must "spend and receive funds in the same transaction" in { wallet =>
    val fundingAddressF = wallet.getNewAddress()
    val receivingAddressF = wallet.getNewAddress()
    val amount = Bitcoins.one

    //build funding tx
    val fundingTxF: Future[(Transaction, UInt32)] = for {
      fundingAddr <- fundingAddressF
      fundingTx = TransactionGenerators.buildCreditingTransaction(
        fundingAddr.scriptPubKey,
        amount)
    } yield fundingTx

    val processedFundingTxF: Future[WalletApi] = for {
      (fundingTx, _) <- fundingTxF
      //make sure wallet is empty
      balance <- wallet.getBalance()
      _ = assert(balance == Bitcoins.zero)
      processed <- wallet.processTransaction(fundingTx,
                                             Some(DoubleSha256DigestBE.empty))
      balance <- wallet.getBalance()
      _ = assert(balance == amount)
    } yield processed

    //build spending tx
    val spendingTxF = for {
      receivingAddress <- receivingAddressF
      (fundingTx, outputIdx) <- fundingTxF
      outPoint = TransactionOutPoint(fundingTx.txIdBE, outputIdx)
      inputs = Vector(
        TransactionInput(outPoint, ScriptSignature.empty, UInt32.zero))
      outputs = Vector(TransactionOutput(amount, receivingAddress.scriptPubKey))
      spendingTx = BaseTransaction(TransactionConstants.version,
                                   inputs,
                                   outputs,
                                   UInt32.zero)
      wallet <- processedFundingTxF
      processedSpendingTx <- wallet.processTransaction(
        transaction = spendingTx,
        blockHash = Some(CryptoGenerators.doubleSha256DigestBE.sampleSome))
      balance <- processedSpendingTx.getBalance()
    } yield assert(balance == amount)

    spendingTxF
  }
}
