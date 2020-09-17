package org.bitcoins.wallet

import org.bitcoins.core.currency._
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTest,
  WalletWithBitcoindRpc,
  WalletWithBitcoindV19
}
import org.scalatest.FutureOutcome

class ProcessBlockTest extends BitcoinSWalletTest {

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNewWalletAndBitcoindV19(test, getBIP39PasswordOpt())

  override type FixtureParam = WalletWithBitcoindV19

  it must "process a block" in { param =>
    val WalletWithBitcoindV19(wallet, bitcoind) = param

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

  it must "process coinbase txs" in { param =>
    val WalletWithBitcoindV19(wallet, bitcoind) = param
    for {
      startingUtxos <- wallet.listUtxos()
      _ = assert(startingUtxos.isEmpty)
      addr <- wallet.getNewAddress()
      hashes <- bitcoind.generateToAddress(101, addr)
      blocks <- FutureUtil.sequentially(hashes)(bitcoind.getBlockRaw)
      _ <- FutureUtil.sequentially(blocks)(wallet.processBlock)
      utxos <- wallet.listUtxos()
      balance <- wallet.getBalance()
    } yield {
      assert(utxos.size == 101)
      assert(balance == Bitcoins(50 * 48) + Bitcoins(25 * 53))
    }
  }

  it must "process coinbase txs using filters" in { param =>
    val WalletWithBitcoindV19(wallet, bitcoind) = param

    for {
      startingUtxos <- wallet.listUtxos()
      _ = assert(startingUtxos.isEmpty)

      addr <- wallet.getNewAddress()
      hashes <- bitcoind.generateToAddress(101, addr)
      filters <- FutureUtil.sequentially(hashes)(
        bitcoind.getBlockFilter(_, FilterType.Basic))
      filtersWithBlockHash = hashes.map(_.flip).zip(filters.map(_.filter))
      _ <- wallet.processCompactFilters(filtersWithBlockHash)
      utxos <- wallet.listUtxos()
      balance <- wallet.getBalance()
    } yield {
      assert(utxos.size == 101)
      assert(balance == Bitcoins(50 * 48) + Bitcoins(25 * 53))
    }
  }
}
