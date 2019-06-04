package org.bitcoins.wallet

import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.wallet.api.{AddUtxoError, AddUtxoSuccess, WalletApi}
import org.bitcoins.wallet.util.BitcoinSWalletTest
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

    val addUtxoF: Future[Unit] = for {
      addr <- wallet.getNewAddress()

      txid <- bitcoind.sendToAddress(addr, valueFromBitcoind)
      _ <- bitcoind.generate(6)
      tx <- bitcoind.getRawTransaction(txid)

      addUtxoRes <- {
        val voutOpt = tx.vout.find { rpcOut =>
          val addressesOpt = rpcOut.scriptPubKey.addresses
          addressesOpt.exists(_.contains(addr))
        }

        val vout = voutOpt.getOrElse(
          throw new IllegalArgumentException(
            "Could not find ouput that spent to our address!"))

        wallet.addUtxo(tx.hex, UInt32(vout.n))
      }
    } yield {
      addUtxoRes match {
        case err: AddUtxoError            => fail(err)
        case AddUtxoSuccess(w: WalletApi) => () // continue test
      }
    }

    for {
      _ <- addUtxoF

      utxos <- wallet.listUtxos()
      _ = assert(utxos.nonEmpty)

      balance <- wallet.getBalance()
      _ = assert(balance > Bitcoins.zero)

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
