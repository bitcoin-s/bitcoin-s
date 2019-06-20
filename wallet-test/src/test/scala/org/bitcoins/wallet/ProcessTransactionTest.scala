package org.bitcoins.wallet

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest.FutureOutcome
import org.bitcoins.core.currency._
import scala.concurrent.Future
import org.scalatest.compatible.Assertion
import org.bitcoins.wallet.api.UnlockedWalletApi
import org.bitcoins.rpc.client.common.BitcoindRpcClient

class ProcessTransactionTest extends BitcoinSWalletTest {
  override type FixtureParam = WalletWithBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withNewWalletAndBitcoind(test)
  }

  behavior of "Wallet.processTransaction"

  /** Verifies that executing the given action doesn't change wallet state */
  private def checkUtxosAndBalance(
      wallet: UnlockedWalletApi,
      bitcoind: BitcoindRpcClient)(action: => Future[_]): Future[Assertion] =
    for {
      oldUtxos <- wallet.listUtxos()
      oldUnconfirmed <- wallet.getUnconfirmedBalance()
      oldConfirmed <- wallet.getBalance()
      _ <- action // by name
      newUtxos <- wallet.listUtxos()
      newUnconfirmed <- wallet.getUnconfirmedBalance()
      newConfirmed <- wallet.getBalance()

    } yield {
      assert(oldConfirmed == newConfirmed)
      assert(oldUnconfirmed == oldUnconfirmed)
      assert(oldUtxos == newUtxos)
    }

  it must "not change state when processing the same transaction twice" in {
    walletAndBitcoind =>
      val WalletWithBitcoind(wallet, bitcoind) = walletAndBitcoind

      for {
        address <- wallet.getNewAddress()
        tx <- bitcoind
          .sendToAddress(address, 1.bitcoin)
          .flatMap(bitcoind.getRawTransactionRaw(_))

        _ <- wallet.processTransaction(tx, confirmations = 0)
        oldBalance <- wallet.getBalance()
        oldUnconfirmed <- wallet.getUnconfirmedBalance()

        // repeating the action should not make a difference
        _ <- checkUtxosAndBalance(wallet, bitcoind) {
          wallet.processTransaction(tx, confirmations = 0)
        }

        _ <- wallet.processTransaction(tx, confirmations = 3)
        newBalance <- wallet.getBalance()
        newUnconfirmed <- wallet.getUnconfirmedBalance()

        // repeating the action should not make a difference
        _ <- checkUtxosAndBalance(wallet, bitcoind) {
          wallet.processTransaction(tx, confirmations = 3)
        }
      } yield {
        assert(newBalance != oldBalance)
        assert(newUnconfirmed != oldUnconfirmed)
      }
  }

  it must "not change state when processing an unrelated transaction" in {
    walletAndBitcoind =>
      val WalletWithBitcoind(wallet, bitcoind) = walletAndBitcoind

      for {
        unrelated <- bitcoind.getNewAddress
          .flatMap(bitcoind.sendToAddress(_, 1.bitcoin))
          .flatMap(bitcoind.getRawTransactionRaw(_))

        _ <- checkUtxosAndBalance(wallet, bitcoind) {
          wallet.processTransaction(unrelated, confirmations = 4)
        }

        balance <- wallet.getBalance()
        unconfirmed <- wallet.getUnconfirmedBalance()

      } yield {
        assert(balance == 0.sats)
        assert(unconfirmed == 0.sats)
      }

  }
}
