package org.bitcoins.wallet

import org.bitcoins.core.currency._
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletWithBitcoindRpc}
import org.scalatest.FutureOutcome

class ProcessBlockTest extends BitcoinSWalletTest {

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNewWalletAndBitcoind(test)

  override type FixtureParam = WalletWithBitcoindRpc

  it must "process a block" in { param =>
    val WalletWithBitcoindRpc(wallet, bitcoind) = param

    for {
      startingUtxos <- wallet.listUtxos()
      _ = assert(startingUtxos.isEmpty)

      addr <- wallet.getNewAddress()
      txId <- bitcoind.sendToAddress(addr, 1.bitcoin)
      hash <-
        bitcoind.getNewAddress
          .flatMap(bitcoind.generateToAddress(1, _))
          .map(_.head)
      block <- bitcoind.getBlockRaw(hash)

      _ <- wallet.processBlock(block)
      utxos <- wallet.listUtxos()
    } yield {
      assert(utxos.size == 1)
      assert(utxos.head.output.scriptPubKey == addr.scriptPubKey)
      assert(utxos.head.output.value == 1.bitcoin)
      assert(utxos.head.blockHash.contains(hash))
      assert(utxos.head.txid == txId)
    }
  }
}
