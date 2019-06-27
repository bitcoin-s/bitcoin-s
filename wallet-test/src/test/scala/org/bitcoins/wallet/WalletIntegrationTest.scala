package org.bitcoins.wallet

import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.wallet.api.{AddUtxoError, AddUtxoSuccess, WalletApi}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest.FutureOutcome

import scala.concurrent.Future

class WalletIntegrationTest extends BitcoinSWalletTest {

  override type FixtureParam = WalletWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNewWalletAndBitcoind(test)

  behavior of "Wallet - integration test"

  val feeRate = SatoshisPerByte(Satoshis.one)

  it should ("create an address, receive funds to it from bitcoind, import the"
    + " UTXO and construct a valid, signed transaction that's"
    + " broadcast and confirmed by bitcoind") in { walletWithBitcoind =>
    val WalletWithBitcoind(wallet, bitcoind) = walletWithBitcoind
    val valueFromBitcoind = Bitcoins.one

    for {
      addr <- wallet.getNewAddress()

      tx <- bitcoind
        .sendToAddress(addr, valueFromBitcoind)
        .flatMap(bitcoind.getRawTransactionRaw(_))

      _ <- wallet.listUtxos().map(utxos => assert(utxos.isEmpty))
      _ <- wallet.getBalance().map(confirmed => assert(confirmed == 0.bitcoin))
      _ <- wallet
        .getUnconfirmedBalance()
        .map(unconfirmed => assert(unconfirmed == 0.bitcoin))

      // after this, tx is unconfirmed in wallet
      _ <- wallet.processTransaction(tx, confirmations = 0)

      utxosPostAdd <- wallet.listUtxos()
      _ = assert(utxosPostAdd.nonEmpty)
      _ <- wallet.getBalance().map(confirmed => assert(confirmed == 0.bitcoin))
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

      addressFromBitcoind <- bitcoind.getNewAddress
      signedTx <- wallet.sendToAddress(addressFromBitcoind,
                                       Bitcoins(0.5),
                                       feeRate)

      txid <- bitcoind.sendRawTransaction(signedTx)
      _ <- bitcoind.generate(1)
      tx <- bitcoind.getRawTransaction(txid)
    } yield {
      assert(tx.confirmations.exists(_ > 0))
    }
  }
}
