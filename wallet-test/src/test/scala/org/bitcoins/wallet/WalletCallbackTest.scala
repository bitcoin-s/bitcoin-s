package org.bitcoins.wallet

import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.P2PKHScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  EmptyTransaction,
  Transaction,
  TransactionOutput
}
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.bitcoins.wallet.models.SpendingInfoDb
import org.scalatest.FutureOutcome

import scala.concurrent.{Future, Promise}

class WalletCallbackTest extends BitcoinSWalletTest {
  type FixtureParam = FundedWallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFundedWallet(test, getBIP39PasswordOpt())
  }

  behavior of "WalletCallbacks"

  it must "verify OnNewAddressGenerated callbacks are executed" in {
    fundedWallet: FundedWallet =>
      val resultP: Promise[BitcoinAddress] = Promise()

      val callback: OnNewAddressGenerated = (addr: BitcoinAddress) => {
        Future {
          resultP.success(addr)
          ()
        }
      }

      val callbacks = WalletCallbacks.onNewAddressGenerated(callback)

      fundedWallet.wallet.walletConfig.addCallbacks(callbacks)

      val wallet = fundedWallet.wallet

      for {
        address <- wallet.getNewAddress()
        exists <- wallet.contains(address, None)
        _ = assert(exists, "Wallet must contain address after generating it")
        result <- resultP.future
      } yield assert(result == address)
  }

  it must "verify OnTransactionProcessed callbacks are executed" in {
    fundedWallet: FundedWallet =>
      val resultP: Promise[Transaction] = Promise()

      val callback: OnTransactionProcessed = (tx: Transaction) => {
        Future {
          resultP.success(tx)
          ()
        }
      }

      val callbacks = WalletCallbacks.onTransactionProcessed(callback)

      fundedWallet.wallet.walletConfig.addCallbacks(callbacks)

      val wallet = fundedWallet.wallet

      for {
        _ <- wallet.processTransaction(EmptyTransaction, None)
        result <- resultP.future
      } yield assert(result == EmptyTransaction)
  }

  it must "verify OnTransactionBroadcast callbacks are executed" in {
    fundedWallet: FundedWallet =>
      val resultP: Promise[Transaction] = Promise()

      val callback: OnTransactionBroadcast = (tx: Transaction) => {
        Future {
          resultP.success(tx)
          ()
        }
      }

      val callbacks = WalletCallbacks.onTransactionBroadcast(callback)

      fundedWallet.wallet.walletConfig.addCallbacks(callbacks)

      val wallet = fundedWallet.wallet

      for {
        _ <- wallet.broadcastTransaction(EmptyTransaction)
        result <- resultP.future
      } yield assert(result == EmptyTransaction)
  }

  private val dummyOutput = TransactionOutput(
    10000.satoshis,
    P2PKHScriptPubKey(ECPublicKey.freshPublicKey))

  it must "verify OnReservedUtxos callbacks are executed when reserving" in {
    fundedWallet: FundedWallet =>
      val resultP: Promise[Vector[SpendingInfoDb]] = Promise()

      val callback: OnReservedUtxos = (infos: Vector[SpendingInfoDb]) => {
        Future {
          resultP.success(infos)
          ()
        }
      }

      val callbacks = WalletCallbacks.onReservedUtxos(callback)

      fundedWallet.wallet.walletConfig.addCallbacks(callbacks)

      val wallet = fundedWallet.wallet

      for {
        utxos <- wallet.listUtxos()
        _ <- wallet.markUTXOsAsReserved(Vector(utxos.head))
        result <- resultP.future
      } yield assert(
        // just compare outPoints because states will be changed so they won't be equal
        result.map(_.outPoint) == Vector(utxos.head).map(_.outPoint))
  }

  it must "verify OnReservedUtxos callbacks are executed when un-reserving" in {
    fundedWallet: FundedWallet =>
      val resultP: Promise[Vector[SpendingInfoDb]] = Promise()

      val callback: OnReservedUtxos = (infos: Vector[SpendingInfoDb]) => {
        Future {
          resultP.success(infos)
          ()
        }
      }

      val callbacks = WalletCallbacks.onReservedUtxos(callback)

      for {
        utxos <- fundedWallet.wallet.listUtxos()
        reserved <- fundedWallet.wallet.markUTXOsAsReserved(Vector(utxos.head))
        _ = fundedWallet.wallet.walletConfig.addCallbacks(callbacks)

        wallet = fundedWallet.wallet

        _ <- wallet.unmarkUTXOsAsReserved(reserved)
        result <- resultP.future
        // just compare outPoints because states will be changed so they won't be equal
      } yield assert(result.map(_.outPoint) == reserved.map(_.outPoint))
  }
}
