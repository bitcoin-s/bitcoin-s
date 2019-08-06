package org.bitcoins.node

import akka.actor.Cancellable
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.node.networking.peer.DataMessageHandler
import org.bitcoins.testkit.node.NodeUnitTest.SpvNodeFundedWalletBitcoind
import org.bitcoins.testkit.node.{NodeTestUtil, NodeUnitTest}
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{BeforeAndAfter, FutureOutcome}

import scala.concurrent._
import scala.concurrent.duration._

class UpdateBloomFilterTest extends NodeUnitTest with BeforeAndAfter {
  override type FixtureParam = SpvNodeFundedWalletBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withSpvNodeFundedWalletBitcoind(test, callbacks)
  }

  val testTimeout = 30.seconds
  private var assertionP: Promise[Boolean] = Promise()
  after {
    //reset assertion after a test runs, because we
    //are doing mutation to work around our callback
    //limitations, we can't currently modify callbacks
    //after a SpvNode is constructed :-(
    assertionP = Promise()
  }

  /** The address we expect to receive funds at */
  private val addressFromWalletP: Promise[BitcoinAddress] = Promise()

  // the TX we sent from our wallet to bitcoind,
  // we expect to get notified once this is
  // confirmed
  private val txFromWalletP: Promise[Transaction] = Promise()

  def addressCallback: DataMessageHandler.OnTxReceived = { tx: Transaction =>
    // we check if any of the addresses in the TX
    // pays to our wallet address
    val _ = for {
      addressFromWallet <- addressFromWalletP.future
      result = tx.outputs.exists(
        _.scriptPubKey == addressFromWallet.scriptPubKey)
    } yield {
      if (result) {
        assertionP.success(true)
      }
    }
  }

  def txCallback: DataMessageHandler.OnMerkleBlockReceived = {
    (_: MerkleBlock, txs: Vector[Transaction]) =>
      {
        txFromWalletP.future
          .map { tx =>
            if (txs.contains(tx)) {
              assertionP.success(true)
            }
          }
      }
  }

  def callbacks: SpvNodeCallbacks = {
    SpvNodeCallbacks(onTxReceived = Vector(addressCallback),
                     onMerkleBlockReceived = Vector(txCallback))
  }

  it must "update the bloom filter with an address" in { param =>
    val SpvNodeFundedWalletBitcoind(initSpv, wallet, rpc) = param

    // we want to schedule a runnable that aborts
    // the test after a timeout, but then
    // we need to cancel that runnable once
    // we get a result
    var cancelable: Option[Cancellable] = None

    for {
      firstBloom <- wallet.getBloomFilter()

      // this has to be generated after our bloom filter
      // is calculated
      addressFromWallet <- wallet.getNewAddress()
      _ = addressFromWalletP.success(addressFromWallet)
      spv <- initSpv.start()
      _ <- spv.updateBloomFilter(addressFromWallet)
      _ <- spv.sync()
      _ <- rpc.sendToAddress(addressFromWallet, 1.bitcoin)
      _ <- NodeTestUtil.awaitSync(spv, rpc)

      _ = {
        cancelable = Some {
          system.scheduler.scheduleOnce(
            testTimeout,
            new Runnable {
              override def run: Unit = {
                if (!assertionP.isCompleted)
                  assertionP.failure(new TestFailedException(
                    s"Did not receive a merkle block message after $testTimeout!",
                    failedCodeStackDepth = 0))
              }
            }
          )
        }
      }
      result <- assertionP.future
    } yield assert(result)
  }

  it must "update the bloom filter with a TX" in { param =>
    val SpvNodeFundedWalletBitcoind(initSpv, wallet, rpc) = param
    // we want to schedule a runnable that aborts
    // the test after a timeout, but then
    // we need to cancel that runnable once
    // we get a result
    var cancelable: Option[Cancellable] = None

    for {
      firstBloom <- wallet.getBloomFilter()

      spv <- initSpv.start()
      addressFromBitcoind <- rpc.getNewAddress
      tx <- wallet
        .sendToAddress(addressFromBitcoind,
                       5.bitcoin,
                       SatoshisPerByte(100.sats))
      _ = txFromWalletP.success(tx)
      updatedBloom <- spv.updateBloomFilter(tx).map(_.bloomFilter)
      _ = spv.broadcastTransaction(tx)
      _ <- spv.sync()
      _ <- NodeTestUtil.awaitSync(spv, rpc)
      _ = assert(updatedBloom.contains(tx.txId))
      _ = {
        cancelable = Some {
          system.scheduler.scheduleOnce(
            testTimeout,
            new Runnable {
              override def run: Unit = {
                if (!assertionP.isCompleted)
                  assertionP.failure(new TestFailedException(
                    s"Did not receive a merkle block message after $testTimeout!",
                    failedCodeStackDepth = 0))
              }
            }
          )
        }
      }
      // this should confirm our TX
      // since we updated the bloom filter
      // we should get notified about the block
      _ <- rpc.getNewAddress.flatMap(rpc.generateToAddress(1, _))

      result <- assertionP.future
    } yield assert(result)

  }
}
