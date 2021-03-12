package org.bitcoins.node

import org.bitcoins.core.currency._
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.{
  NodeTestUtil,
  NodeTestWithCachedBitcoindNewest,
  SpvNodeFundedWalletBitcoind
}
import org.scalatest.{BeforeAndAfter, FutureOutcome, Outcome}

import scala.concurrent.Future

class UpdateBloomFilterTest
    extends NodeTestWithCachedBitcoindNewest
    with BeforeAndAfter {

  /** Wallet config with data directory set to user temp directory */
  implicit override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = SpvNodeFundedWalletBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val outcome: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      outcome = withSpvNodeFundedWalletBitcoindCached(test,
                                                      getBIP39PasswordOpt(),
                                                      bitcoind)
      f <- outcome.toFuture
    } yield f
    new FutureOutcome(outcome)
  }

  it must "update the bloom filter with a TX" in { param =>
    val SpvNodeFundedWalletBitcoind(spv, wallet, rpc, _) = param

    for {
      _ <- wallet.getBloomFilter()
      tx <- wallet.sendToAddress(junkAddress, 5.bitcoin, None)
      updatedBloom <- spv.updateBloomFilter(tx).map(_.bloomFilter)
      _ = assert(updatedBloom.contains(tx.txId))
      _ <- rpc.broadcastTransaction(tx)

      // this should confirm our TX
      // since we updated the bloom filter
      hash <- rpc.generateToAddress(1, junkAddress).map(_.head)
      _ <- NodeTestUtil.awaitSync(spv, rpc)
      merkleBlock <- rpc.getTxOutProof(Vector(tx.txIdBE), hash)
      txs <- rpc.verifyTxOutProof(merkleBlock)

    } yield assert(txs.contains(tx.txIdBE))
  }

  it must "update the bloom filter with an address" in { param =>
    val SpvNodeFundedWalletBitcoind(spv, wallet, rpc, _) = param

    for {
      _ <- wallet.getBloomFilter()

      address <- wallet.getNewAddress()
      updatedBloom <- spv.updateBloomFilter(address).map(_.bloomFilter)
      hash <- rpc.sendToAddress(address, 1.bitcoin)
      tx <- rpc.getRawTransactionRaw(hash)
    } yield assert(updatedBloom.isRelevant(tx))
  }
}
