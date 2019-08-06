package org.bitcoins.wallet

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.api.UnlockedWalletApi
import org.scalatest.FutureOutcome
import org.bitcoins.wallet.api.UnlockWalletError._
import org.bitcoins.wallet.api.UnlockWalletSuccess
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.currency._
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.wallet.BitcoinSWalletTest.WalletWithBitcoind

class WalletBloomTest extends BitcoinSWalletTest {
  behavior of "Wallet bloom filter"

  override type FixtureParam = WalletWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withFundedWalletAndBitcoind(test)

  it should "generate a bloom filter that matches the pubkeys in our wallet" in {
    param =>
      val WalletWithBitcoind(walletApi, _) = param
      val wallet = walletApi.asInstanceOf[Wallet]
      for {
        _ <- FutureUtil.sequentially(0 until 10)(_ => wallet.getNewAddress())
        bloom <- wallet.getBloomFilter()
        pubkeys <- wallet.listPubkeys()
      } yield {
        pubkeys.map { (pub) =>
          assert(bloom.contains(pub))
        }.toAssertion
      }
  }

  it should "generate a bloom filter that matches the outpoints in our wallet" in {
    param =>
      val WalletWithBitcoind(walletApi, bitcoind) = param
      val wallet = walletApi.asInstanceOf[Wallet]

      for {
        outpoints <- wallet.listOutpoints()

        bloom <- wallet.getBloomFilter()
      } yield {
        outpoints.map { (out) =>
          assert(bloom.contains(out))
        }.toAssertion
      }
  }
}
