package org.bitcoins.wallet

import org.bitcoins.commons.jsonmodels.wallet.SyncHeightDescriptor
import org.bitcoins.core.currency._
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.testkit.rpc.CachedBitcoindV19
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTestCachedBitcoinV19,
  WalletWithBitcoindV19
}
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

class ProcessBlockTest
    extends BitcoinSWalletTestCachedBitcoinV19
    with CachedBitcoindV19 {

  override type FixtureParam = WalletWithBitcoindV19

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withNewWalletAndBitcoindCachedV19(test,
                                                     getBIP39PasswordOpt(),
                                                     bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

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

      height <- bitcoind.getBlockCount
      bestHash <- bitcoind.getBestBlockHash
      syncHeightOpt <- wallet.getSyncDescriptorOpt()
      txDbOpt <- wallet.transactionDAO.findByTxId(txId)
    } yield {
      assert(txDbOpt.isDefined)
      assert(txDbOpt.get.blockHashOpt.contains(hash))
      assert(utxos.size == 1)
      assert(utxos.head.output.scriptPubKey == addr.scriptPubKey)
      assert(utxos.head.output.value == 1.bitcoin)
      assert(utxos.head.txid == txId)

      assert(syncHeightOpt.contains(SyncHeightDescriptor(bestHash, height)))
    }
  }

  it must "process coinbase txs" in { param =>
    val WalletWithBitcoindV19(wallet, bitcoind) = param
    for {
      startingUtxos <- wallet.listUtxos(TxoState.ImmatureCoinbase)
      startingBalance <- wallet.getBalance()
      _ = assert(startingUtxos.isEmpty)
      _ = assert(startingBalance == Satoshis.zero)
      addr <- wallet.getNewAddress()
      hashes <- bitcoind.generateToAddress(101, addr)
      blocks <- FutureUtil.sequentially(hashes)(bitcoind.getBlockRaw)
      _ <- FutureUtil.sequentially(blocks)(wallet.processBlock)
      utxos <- wallet.listUtxos(TxoState.ImmatureCoinbase)
      balance <- wallet.getBalance()

      height <- bitcoind.getBlockCount
      bestHash <- bitcoind.getBestBlockHash
      syncHeightOpt <- wallet.getSyncDescriptorOpt()
    } yield {
      assert(utxos.size == 100)
      assert(balance == Bitcoins(50))

      assert(syncHeightOpt.contains(SyncHeightDescriptor(bestHash, height)))
    }
  }

  it must "process coinbase txs using filters" in { param =>
    val WalletWithBitcoindV19(wallet, bitcoind) = param

    for {
      startingUtxos <- wallet.listUtxos(TxoState.ImmatureCoinbase)
      startingBalance <- wallet.getBalance()
      _ = assert(startingUtxos.isEmpty)
      _ = assert(startingBalance == Satoshis.zero)
      addr <- wallet.getNewAddress()
      hashes <- bitcoind.generateToAddress(102, addr)
      filters <- FutureUtil.sequentially(hashes)(
        bitcoind.getBlockFilter(_, FilterType.Basic))
      filtersWithBlockHash = hashes.map(_.flip).zip(filters.map(_.filter))
      _ <- wallet.processCompactFilters(filtersWithBlockHash)
      utxos <- wallet.listUtxos(TxoState.ImmatureCoinbase)
      balance <- wallet.getBalance()

      height <- bitcoind.getBlockCount
      bestHash <- bitcoind.getBestBlockHash
      syncHeightOpt <- wallet.getSyncDescriptorOpt()
    } yield {
      assert(utxos.size == 100)
      assert(balance == Bitcoins(50))

      assert(syncHeightOpt.contains(SyncHeightDescriptor(bestHash, height)))
    }
  }
}
