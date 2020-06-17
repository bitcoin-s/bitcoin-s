package org.bitcoins.wallet

import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.script.P2PKHScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  EmptyTransaction,
  Transaction,
  TransactionOutput
}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.bitcoins.wallet.models.{AddressDb, SpendingInfoDb}
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
      val resultP: Promise[Boolean] = Promise()

      val callback: OnNewAddressGenerated = (_: AddressDb) => {
        Future {
          resultP.success(true)
          ()
        }
      }

      val callbacks = WalletCallbacks.onNewAddressGenerated(callback)

      val wallet = fundedWallet.wallet.addCallbacks(callbacks)

      for {
        address <- wallet.getNewAddress()
        exists <- wallet.contains(address, None)
        _ = assert(exists, "Wallet must contain address after generating it")
        result <- resultP.future
      } yield assert(result)
  }

  it must "verify OnTransactionProcessed callbacks are executed" in {
    fundedWallet: FundedWallet =>
      val resultP: Promise[Boolean] = Promise()

      val callback: OnTransactionProcessed = (_: Transaction) => {
        Future {
          resultP.success(true)
          ()
        }
      }

      val callbacks = WalletCallbacks.onTransactionProcessed(callback)

      val wallet = fundedWallet.wallet.addCallbacks(callbacks)

      for {
        _ <- wallet.processTransaction(EmptyTransaction, None)
        result <- resultP.future
      } yield assert(result)
  }

  it must "verify OnTransactionBroadcast callbacks are executed" in {
    fundedWallet: FundedWallet =>
      val resultP: Promise[Boolean] = Promise()

      val callback: OnTransactionBroadcast = (_: Transaction) => {
        Future {
          resultP.success(true)
          ()
        }
      }

      val callbacks = WalletCallbacks.onTransactionBroadcast(callback)

      val wallet = fundedWallet.wallet.addCallbacks(callbacks)

      for {
        _ <- wallet.broadcastTransaction(EmptyTransaction)
        result <- resultP.future
      } yield assert(result)
  }

  private val dummyOutput = TransactionOutput(
    10000.satoshis,
    P2PKHScriptPubKey(ECPublicKey.freshPublicKey))

  it must "verify OnReservedUtxos callbacks are executed when reserving" in {
    fundedWallet: FundedWallet =>
      val resultP: Promise[Boolean] = Promise()

      val callback: OnReservedUtxos = (_: Vector[SpendingInfoDb]) => {
        Future {
          resultP.success(true)
          ()
        }
      }

      val callbacks = WalletCallbacks.onReservedUtxos(callback)

      val wallet = fundedWallet.wallet.addCallbacks(callbacks)

      for {
        _ <- wallet.fundRawTransaction(Vector(dummyOutput),
                                       SatoshisPerVirtualByte.one,
                                       markAsReserved = true)
        result <- resultP.future
      } yield assert(result)
  }

  it must "verify OnReservedUtxos callbacks are executed when un-reserving" in {
    fundedWallet: FundedWallet =>
      val resultP: Promise[Boolean] = Promise()

      val callback: OnReservedUtxos = (_: Vector[SpendingInfoDb]) => {
        Future {
          resultP.success(true)
          ()
        }
      }

      val callbacks = WalletCallbacks.onReservedUtxos(callback)

      for {
        tx <- fundedWallet.wallet.fundRawTransaction(Vector(dummyOutput),
                                                     SatoshisPerVirtualByte.one,
                                                     markAsReserved = true)
        wallet = fundedWallet.wallet.addCallbacks(callbacks)

        utxos <- wallet.spendingInfoDAO.findOutputsBeingSpent(tx)
        _ <- wallet.unmarkUTXOsAsReserved(utxos.toVector)
        result <- resultP.future
      } yield assert(result)
  }
}
