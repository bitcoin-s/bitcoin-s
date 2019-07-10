package org.bitcoins.wallet

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.api.UnlockedWalletApi
import org.scalatest.FutureOutcome
import org.bitcoins.wallet.api.UnlockWalletError._
import org.bitcoins.wallet.api.UnlockWalletSuccess
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.currency._
import org.bitcoins.testkit.Implicits._

class WalletBloomTest extends BitcoinSWalletTest {
  behavior of "Wallet bloom filter"

  override type FixtureParam = WalletWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNewWalletAndBitcoind(test)

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

  // TODO: change fixture to withFundedWalletAndBitcoind once #577 goes in
  // https://github.com/bitcoin-s/bitcoin-s/pull/577/files#diff-0fb6ac004fe1e550b7c13258d7d0706cR154
  it should "generate a bloom filter that matches the outpoints in our wallet" in {
    param =>
      val WalletWithBitcoind(walletApi, bitcoind) = param
      val wallet = walletApi.asInstanceOf[Wallet]

      for {
        address <- wallet.getNewAddress()
        tx <- bitcoind
          .sendToAddress(address, 5.bitcoins)
          .flatMap(bitcoind.getRawTransaction(_))
        _ <- wallet.processTransaction(tx.hex, confirmations = 0)
        outpoints <- wallet.listOutpoints()

        bloom <- wallet.getBloomFilter()
      } yield {
        outpoints.map { (out) =>
          assert(bloom.contains(out))
        }.toAssertion
      }
  }
}
