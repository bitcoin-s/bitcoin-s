package org.bitcoins.node

import akka.actor.Cancellable
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.currency._
import org.bitcoins.node.networking.peer.DataMessageHandler
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.fixtures.UsesExperimentalBitcoind
import org.bitcoins.testkit.node.NodeUnitTest.NeutrinoNodeFundedWalletBitcoind
import org.bitcoins.testkit.node.{NodeTestUtil, NodeUnitTest}
import org.bitcoins.wallet.api.UnlockedWalletApi
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{DoNotDiscover, FutureOutcome}

import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

@DoNotDiscover
class NeutrinoNodeWithWalletTest extends NodeUnitTest {

  /** Wallet config with data directory set to user temp directory */
  implicit override protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoTestConfig()

  override type FixtureParam = NeutrinoNodeFundedWalletBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withNeutrinoNodeFundedWalletBitcoind(test,
                                         callbacks,
                                         Some(BitcoindVersion.Experimental))
  }

  private val assertionP: Promise[Boolean] = Promise()

  private val walletP: Promise[UnlockedWalletApi] = Promise()
  private val walletF: Future[UnlockedWalletApi] = walletP.future

  val amountFromBitcoind = 1.bitcoin

  def callbacks: SpvNodeCallbacks = {
    val onBlock: DataMessageHandler.OnBlockReceived = { block =>
      for {
        wallet <- walletF
      } yield {
        for {
          prevBalance <- wallet.getUnconfirmedBalance()
          _ <- wallet.processBlock(block, confirmations = 0)
          balance <- wallet.getUnconfirmedBalance()
        } yield {
          val result = balance == prevBalance + amountFromBitcoind
          assertionP.success(result)
        }
      }
    }
    SpvNodeCallbacks(
      onBlockReceived = Seq(onBlock)
    )
  }

  it must "rescan and receive information about received payments" taggedAs (UsesExperimentalBitcoind) in {
    param =>
      val NeutrinoNodeFundedWalletBitcoind(node, wallet, bitcoind) = param

      walletP.success(wallet)

      var cancellable: Option[Cancellable] = None

      def setTimeout(tx: DoubleSha256DigestBE): DoubleSha256DigestBE = {
        // how long we're waiting for a tx notify before failing the test
        val delay = 25.seconds

        val failTest: Runnable = new Runnable {
          override def run = {
            if (!assertionP.isCompleted) {
              val msg =
                s"Did not receive a block within $delay"
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
        _ <- node.sync()
        _ <- NodeTestUtil.awaitSync(node, bitcoind)
        _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)

        address <- wallet.getNewAddress()
        _ <- bitcoind
          .sendToAddress(address, amountFromBitcoind)
          .map(setTimeout)

        _ <- bitcoind.getNewAddress
          .flatMap(bitcoind.generateToAddress(1, _))
        _ <- NodeTestUtil.awaitSync(node, bitcoind)
        _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)

        _ <- node.rescan(Vector(address.scriptPubKey))

        result <- assertionP.future
      } yield assert(result)

  }
}
