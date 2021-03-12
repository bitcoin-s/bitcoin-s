package org.bitcoins.node

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.currency._
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.{
  NodeTestUtil,
  NodeTestWithCachedBitcoindNewest,
  SpvNodeFundedWalletBitcoind
}
import org.bitcoins.wallet.Wallet
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

class SpvNodeWithWalletTest extends NodeTestWithCachedBitcoindNewest {

  /** Wallet config with data directory set to user temp directory */
  implicit override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = SpvNodeFundedWalletBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val outcomeF: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      outcome = withSpvNodeFundedWalletBitcoindCached(test,
                                                      getBIP39PasswordOpt(),
                                                      bitcoind)
      f <- outcome.toFuture
    } yield f
    new FutureOutcome(outcomeF)
  }

  val amountFromBitcoind = 1.bitcoin

  it must "load a bloom filter and receive information about received payments" in {
    param =>
      val SpvNodeFundedWalletBitcoind(spv, wallet, rpc, _) = param

      for {
        _ <- wallet.getBloomFilter()
        address <- wallet.getNewAddress()
        updatedBloom <- spv.updateBloomFilter(address).map(_.bloomFilter)

        ourTxid <-
          rpc
            .sendToAddress(address, amountFromBitcoind)
        _ <- rpc.generateToAddress(1, junkAddress)
        _ <- spv.sync()
        _ <- NodeTestUtil.awaitSync(spv, rpc)

        ourTx <- rpc.getTransaction(ourTxid)

        _ = assert(updatedBloom.isRelevant(ourTx.hex))
        //wait for bitcoind to propagate us a merkle block
        //and transactions associated with it
        //eventually we should have the tx
        //added to our wallet when this occurs
        _ <- AsyncUtil.retryUntilSatisfiedF(() =>
          walletContainsTx(wallet, ourTx.txid))
      } yield {
        succeed
      }
  }

  private def walletContainsTx(
      wallet: Wallet,
      txid: DoubleSha256DigestBE): Future[Boolean] = {
    val txOptF = wallet.findTransaction(txid)
    for {
      txOpt <- txOptF
    } yield txOpt.isDefined
  }
}
