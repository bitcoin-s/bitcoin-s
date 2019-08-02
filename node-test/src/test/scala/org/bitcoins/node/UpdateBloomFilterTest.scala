package org.bitcoins.node

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest.FutureOutcome
import org.bitcoins.node.models.Peer
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.testkit.node.NodeTestUtil
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.networking.peer.DataMessageHandler
import org.bitcoins.core.protocol.transaction.Transaction
import scala.concurrent._
import scala.concurrent.duration._
import org.scalatest.compatible.Assertion
import org.bitcoins.core.currency._
import scala.util.Try
import akka.actor.Cancellable
import org.scalatest.run
import org.scalatest.exceptions.TestFailedException
import org.bitcoins.core.wallet.fee.SatoshisPerByte

class UpdateBloomFilterTest extends BitcoinSWalletTest {
  override type FixtureParam = WalletWithBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withFundedWalletAndBitcoind(test)

  it must "update the bloom filter with an address" in { param =>
    val WalletWithBitcoind(wallet, rpc) = param
    implicit val chainConf: ChainAppConfig = config
    implicit val nodeConf: NodeAppConfig = config

    val assertionP = Promise[Assertion]
    val assertionF = assertionP.future

    // we want to schedule a runnable that aborts
    // the test after a timeout, but then
    // we need to cancel that runnable once
    // we get a result
    var cancelable: Option[Cancellable] = None
    val timeout = 15.seconds

    for {
      _ <- config.initialize()

      firstBloom <- wallet.getBloomFilter()

      // this has to be generated after our bloom filter
      // is calculated
      addressFromWallet <- wallet.getNewAddress()

      spv <- {
        val callback = SpvNodeCallbacks.onTxReceived { tx =>
          rpc.getRawTransaction(tx.txIdBE).foreach { res =>
            val paysToOurAddress =
              // we check if any of the addresses in the TX
              // pays to our wallet address
              res.vout.exists(_.scriptPubKey.addresses match {
                case None            => false
                case Some(addresses) => addresses.exists(_ == addressFromWallet)
              })
            cancelable.forall(_.cancel())
            assertionP.complete {
              Try {
                assert(paysToOurAddress)
              }
            }
          }
        }

        val peer = Peer.fromBitcoind(rpc.instance)
        val chain = {
          val dao = BlockHeaderDAO()
          ChainHandler(dao)
        }
        val spv =
          SpvNode(peer, chain, bloomFilter = firstBloom, callbacks = callback)
        spv.start()
      }
      _ <- spv.sync()
      _ <- NodeTestUtil.awaitSync(spv, rpc)

      _ = spv.updateBloomFilter(addressFromWallet)
      _ = {
        val runnable = new Runnable {
          override def run: Unit = {
            assertionP.failure(
              new TestFailedException(
                s"Did not receive a TX message after $timeout!",
                failedCodeStackDepth = 0))
          }
        }
        cancelable = Some {
          actorSystem.scheduler.scheduleOnce(timeout, runnable)
        }
      }
      _ <- rpc.sendToAddress(addressFromWallet, 1.bitcoin)
      assertion <- assertionF
    } yield assertion
  }

  it must "update the bloom filter with a TX" in { param =>
    val WalletWithBitcoind(wallet, rpc) = param
    implicit val chainConf: ChainAppConfig = config
    implicit val nodeConf: NodeAppConfig = config

    val assertionP = Promise[Assertion]
    val assertionF = assertionP.future

    // we want to schedule a runnable that aborts
    // the test after a timeout, but then
    // we need to cancel that runnable once
    // we get a result
    var cancelable: Option[Cancellable] = None

    // the TX we sent from our wallet to bitcoind,
    // we expect to get notified once this is
    // confirmed
    var txFromWallet: Option[Transaction] = None
    val timeout = 15.seconds

    for {
      _ <- config.initialize()

      firstBloom <- wallet.getBloomFilter()

      spv <- {
        val callback = SpvNodeCallbacks.onMerkleBlockReceived { (block, txs) =>
          val isFromOurWallet = txFromWallet.exists(tx => txs.contains(tx))
          // we might receive more merkle blocks than just the
          // one for our TX
          if (isFromOurWallet) {
            assertionP.success(assert(isFromOurWallet))
          }
        }

        val peer = Peer.fromBitcoind(rpc.instance)
        val chain = {
          val dao = BlockHeaderDAO()
          ChainHandler(dao)
        }
        val spv =
          SpvNode(peer, chain, bloomFilter = firstBloom, callbacks = callback)
        spv.start()
      }
      _ <- spv.sync()
      _ <- NodeTestUtil.awaitSync(spv, rpc)

      addressFromBitcoind <- rpc.getNewAddress
      tx <- wallet
        .sendToAddress(addressFromBitcoind,
                       5.bitcoin,
                       SatoshisPerByte(100.sats))
        .map { tx =>
          txFromWallet = Some(tx)
          tx
        }

      _ = {
        val _ = spv.broadcastTransaction(tx)
        val SpvNode(_, _, newBloom, _) = spv.updateBloomFilter(tx)
        assert(newBloom.contains(tx.txId))

        cancelable = Some {
          actorSystem.scheduler.scheduleOnce(
            timeout,
            new Runnable {
              override def run: Unit = {
                if (!assertionP.isCompleted)
                  assertionP.failure(
                    new TestFailedException(
                      s"Did not receive a merkle block message after $timeout!",
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

      assertion <- assertionF
    } yield assertion

  }
}
