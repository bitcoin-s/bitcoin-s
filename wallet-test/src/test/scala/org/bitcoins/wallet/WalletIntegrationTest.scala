package org.bitcoins.wallet

import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.wallet.api.{AddUtxoError, AddUtxoSuccess, WalletApi}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest.FutureOutcome

import scala.concurrent.Future
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.testkit.wallet.BitcoinSWalletTest.WalletWithBitcoind

class WalletIntegrationTest extends BitcoinSWalletTest {

  override type FixtureParam = WalletWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNewWalletAndBitcoind(test)

  behavior of "Wallet - integration test"

  val feeRate = SatoshisPerByte(Satoshis.one)

  /** Checks that the given vaues are the same-ish, save for fee-level deviations */
  private def isCloseEnough(
      first: CurrencyUnit,
      second: CurrencyUnit,
      delta: CurrencyUnit = 1000.sats): Boolean = {
    val diff =
      if (first > second) {
        first - second
      } else if (first < second) {
        second - first
      } else 0.sats

    diff < delta
  }

  it should ("create an address, receive funds to it from bitcoind, import the"
    + " UTXO and construct a valid, signed transaction that's"
    + " broadcast and confirmed by bitcoind") in { walletWithBitcoind =>
    val WalletWithBitcoind(wallet, bitcoind) = walletWithBitcoind
    // the amount we're receiving from bitcoind
    val valueFromBitcoind = Bitcoins.one

    // the amount we're sending to bitcoind
    val valueToBitcoind = Bitcoins(0.5)

    for {
      addr <- wallet.getNewAddress()

      tx <- bitcoind
        .sendToAddress(addr, valueFromBitcoind)
        .flatMap(bitcoind.getRawTransactionRaw(_))

      // before processing TX, wallet should be completely empty
      _ <- wallet.listUtxos().map(utxos => assert(utxos.isEmpty))
      _ <- wallet.getBalance().map(confirmed => assert(confirmed == 0.bitcoin))
      _ <- wallet
        .getUnconfirmedBalance()
        .map(unconfirmed => assert(unconfirmed == 0.bitcoin))

      // after this, tx is unconfirmed in wallet
      _ <- wallet.processTransaction(tx, confirmations = 0)

      // we should now have one UTXO in the wallet
      // it should not be confirmed
      utxosPostAdd <- wallet.listUtxos()
      _ = assert(utxosPostAdd.length == 1)
      _ <- wallet
        .getConfirmedBalance()
        .map(confirmed => assert(confirmed == 0.bitcoin))
      _ <- wallet
        .getConfirmedBalance()
        .map(confirmed => assert(confirmed == 0.bitcoin))
      _ <- wallet
        .getUnconfirmedBalance()
        .map(unconfirmed => assert(unconfirmed == valueFromBitcoind))

      // after this, tx should be confirmed
      _ <- wallet.processTransaction(tx, confirmations = 6)
      _ <- wallet
        .listUtxos()
        .map { utxos =>
          // we want to make sure no new utxos were added,
          // i.e. that we only modified an existing one
          assert(utxos.length == utxosPostAdd.length)
        }

      _ <- wallet
        .getBalance()
        .map(confirmed => assert(confirmed == valueFromBitcoind))
      _ <- wallet
        .getUnconfirmedBalance()
        .map(unconfirmed => assert(unconfirmed == 0.bitcoin))

      signedTx <- bitcoind.getNewAddress.flatMap {
        wallet.sendToAddress(_, valueToBitcoind, feeRate)
      }

      txid <- bitcoind.sendRawTransaction(signedTx)
      _ <- bitcoind.generate(1)
      tx <- bitcoind.getRawTransaction(txid)

      _ <- wallet.listUtxos().map {
        case utxo +: Vector() =>
          assert(utxo.privKeyPath.chain.chainType == HDChainType.Change)
        case other => fail(s"Found ${other.length} utxos!")
      }

      balancePostSend <- wallet.getBalance()
      _ = {
        // change UTXO should be smaller than what we had, but still have money in it
        assert(balancePostSend > 0.sats)
        assert(balancePostSend < valueFromBitcoind)

        assert(
          isCloseEnough(balancePostSend, valueFromBitcoind - valueToBitcoind))
      }

    } yield {
      assert(tx.confirmations.exists(_ > 0))
    }
  }
}
