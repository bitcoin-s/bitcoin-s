package org.bitcoins.node

import akka.actor.Cancellable
import org.bitcoins.core.currency._
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.NodeUnitTest.SpvNodeFundedWalletBitcoind
import org.bitcoins.testkit.node.{NodeTestUtil, NodeUnitTest}
import org.bitcoins.wallet.api.WalletApi
import org.scalatest.FutureOutcome
import org.scalatest.exceptions.TestFailedException

import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

class SpvNodeWithWalletTest extends NodeUnitTest {

  /** Wallet config with data directory set to user temp directory */
  implicit override protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = SpvNodeFundedWalletBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withSpvNodeFundedWalletBitcoind(test, callbacks)
  }

  private val assertionP: Promise[Boolean] = Promise()

  private val expectedTxIdP: Promise[DoubleSha256Digest] = Promise()
  private val expectedTxIdF: Future[DoubleSha256Digest] = expectedTxIdP.future

  private val walletP: Promise[WalletApi] = Promise()
  private val walletF: Future[WalletApi] = walletP.future

  val amountFromBitcoind = 1.bitcoin

  def callbacks: NodeCallbacks = {
    val onTx: OnTxReceived = { tx =>
      for {
        expectedTxId <- expectedTxIdF
        wallet <- walletF
      } yield {
        if (expectedTxId == tx.txId) {
          for {
            prevBalance <- wallet.getUnconfirmedBalance()
            _ <- wallet.processTransaction(tx, None)
            balance <- wallet.getUnconfirmedBalance()
          } yield {
            val result = balance == prevBalance + amountFromBitcoind
            assertionP.success(result)
          }
        }
      }
    }
    NodeCallbacks(
      onTxReceived = Vector(onTx)
    )
  }

  it must "load a bloom filter and receive information about received payments" in {
    param =>
      val SpvNodeFundedWalletBitcoind(spv, wallet, rpc) = param

      walletP.success(wallet)

      var cancellable: Option[Cancellable] = None

      def processWalletTx(tx: DoubleSha256DigestBE): DoubleSha256DigestBE = {
        expectedTxIdP.success(tx.flip)
        // how long we're waiting for a tx notify before failing the test
        val delay = 25.seconds

        val failTest: Runnable = new Runnable {
          override def run = {
            if (!assertionP.isCompleted) {
              val msg =
                s"Did not receive sent transaction within $delay"
              logger.error(msg)
              assertionP.failure(new TestFailedException(msg, 0))
            }
          }
        }

        logger.debug(s"Setting timeout for receiving TX through node")
        cancellable = Some(system.scheduler.scheduleOnce(delay, failTest))
        tx
      }

      for {
        _ <- wallet.getBloomFilter()
        address <- wallet.getNewAddress()
        updatedBloom <- spv.updateBloomFilter(address).map(_.bloomFilter)
        _ <- spv.sync()
        _ <- NodeTestUtil.awaitSync(spv, rpc)

        ourTxid <- rpc
          .sendToAddress(address, amountFromBitcoind)
          .map(processWalletTx)

        ourTx <- rpc.getTransaction(ourTxid)
        _ = assert(updatedBloom.isRelevant(ourTx.hex))

        result <- assertionP.future
      } yield assert(result)

  }
}
