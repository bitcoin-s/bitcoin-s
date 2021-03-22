package org.bitcoins.wallet

import org.bitcoins.core.util.FutureUtil
import org.bitcoins.testkitcore.Implicits._
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTestCachedBitcoindNewest,
  WalletWithBitcoind,
  WalletWithBitcoindRpc
}
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

class WalletBloomTest extends BitcoinSWalletTestCachedBitcoindNewest {
  behavior of "Wallet bloom filter"

  override type FixtureParam = WalletWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withFundedWalletAndBitcoindCached(test,
                                                     getBIP39PasswordOpt(),
                                                     bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  it should "generate a bloom filter that matches the pubkeys in our wallet" in {
    param =>
      val WalletWithBitcoindRpc(walletApi, _) = param
      val wallet = walletApi.asInstanceOf[Wallet]
      for {
        _ <- FutureUtil.sequentially(0 until 10)(_ => wallet.getNewAddress())
        bloom <- wallet.getBloomFilter()
        pubkeys <- wallet.listPubkeys()
      } yield {
        pubkeys.map { pub =>
          assert(bloom.contains(pub))
        }.toAssertion
      }
  }

  it should "generate a bloom filter that matches the outpoints in our wallet" in {
    param =>
      val WalletWithBitcoindRpc(walletApi, _) = param
      val wallet = walletApi.asInstanceOf[Wallet]

      for {
        outpoints <- wallet.listOutpoints()

        bloom <- wallet.getBloomFilter()
      } yield {
        outpoints.map { out =>
          assert(bloom.contains(out))
        }.toAssertion
      }
  }
}
