package org.bitcoins.wallet

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.{Block, RegTestNetChainParams}
import org.bitcoins.core.protocol.transaction.{EmptyTransaction, Transaction}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.scalatest.FutureOutcome

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}

class WalletCallbackTest extends BitcoinSWalletTest {
  type FixtureParam = FundedWallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFundedWallet(test, getBIP39PasswordOpt())(getFreshWalletAppConfig)
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
        address <- wallet.getNewAddress()
        tx <- wallet.sendToAddress(address, Satoshis(1000), None)
        _ <- wallet.processTransaction(tx, None)
        result <- resultP.future
      } yield assert(result == tx)
  }

  it must "verify OnTransactionProcessed callbacks are not executed for a transaction unrelated to the wallet" in {
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

      // a random testnet transaction
      val tx = Transaction.fromHex(
        """02000000000101c2cb8b4d16d2a111cfd2f44e674a89327cfb2dcad5828ec9ad12edb3972b2c
          |b20100000000feffffff023ef929e900000000160014a5f44222c5859b388f513f07c96bdf69
          |8a5a6bfd87c71d00000000001600145543e613b22f2393e76510cede73952405a5c9b9024730
          |440220348dc443d9a0cc6b5365d7ef8d62e1ca4d890c6f4d817a0fb0f48ff36b97e08702201d
          |77554641889932523e7d103385d99834cb9f29328ce11282ccbe218acf56440121028bb78dbe
          |0ea469c97061b8dcc870ec25d5abcd938f19ec17e32422f8f318fa251b992000""".stripMargin)

      for {
        txno <- wallet.listTransactions().map(_.size)
        _ <- wallet.processTransaction(tx, None)
        _ <- AsyncUtil.nonBlockingSleep(50.millis)
        txs <- wallet.listTransactions()
      } yield {
        assert(txs.size == txno)
        assert(!resultP.isCompleted)
      }
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

      val wallet = fundedWallet.wallet

      for {
        utxos <- wallet.listUtxos()
        reserved <- wallet.markUTXOsAsReserved(Vector(utxos.head))
        _ = fundedWallet.wallet.walletConfig.addCallbacks(callbacks)

        _ <- wallet.unmarkUTXOsAsReserved(reserved)
        result <- resultP.future
        // just compare outPoints because states will be changed so they won't be equal
      } yield assert(result.map(_.outPoint) == reserved.map(_.outPoint))
  }

  it must "verify OnBlockProcessed callbacks are executed" in {
    fundedWallet: FundedWallet =>
      val resultP: Promise[Block] = Promise()
      val block = RegTestNetChainParams.genesisBlock
      val callback: OnBlockProcessed = (b: Block) => {
        Future {
          resultP.success(b)
          ()
        }
      }

      val callbacks = WalletCallbacks.onBlockProcessed(callback)

      fundedWallet.wallet.walletConfig.addCallbacks(callbacks)

      val wallet = fundedWallet.wallet

      for {
        _ <- wallet.processBlock(block)
        result <- resultP.future
      } yield assert(result == block)
  }
}
