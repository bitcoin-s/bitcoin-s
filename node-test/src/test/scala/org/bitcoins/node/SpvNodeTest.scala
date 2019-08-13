package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.util.{AsyncUtil, RpcUtil}
import org.bitcoins.testkit.node.NodeUnitTest
import org.bitcoins.testkit.node.fixture.SpvNodeConnectedWithBitcoind
import org.scalatest.FutureOutcome

import scala.concurrent.Future
import org.bitcoins.testkit.node.NodeTestUtil

import scala.concurrent.duration.DurationInt

class SpvNodeTest extends NodeUnitTest {

  override type FixtureParam = SpvNodeConnectedWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withSpvNodeConnectedToBitcoind(test)

  behavior of "SpvNode"

  it must "receive notification that a block occurred on the p2p network" in {
    spvNodeConnectedWithBitcoind: SpvNodeConnectedWithBitcoind =>
      val spvNode = spvNodeConnectedWithBitcoind.spvNode
      val bitcoind = spvNodeConnectedWithBitcoind.bitcoind

      val assert1F = for {
        _ <- spvNode.isConnected.map(assert(_))
        a2 <- spvNode.isInitialized.map(assert(_))
      } yield a2

      val hashF: Future[DoubleSha256DigestBE] = {
        bitcoind.generate(1).map(_.head)
      }

      //sync our spv node expecting to get that generated hash
      val spvSyncF = for {
        _ <- assert1F
        _ <- hashF
        sync <- spvNode.sync()
      } yield sync

      spvSyncF.flatMap { _ =>
        NodeTestUtil
          .awaitSync(spvNode, bitcoind)
          .map(_ => succeed)
      }
  }

  it must "stay in sync with a bitcoind instance" in {
    spvNodeConnectedWithBitcoind: SpvNodeConnectedWithBitcoind =>
      val spvNode = spvNodeConnectedWithBitcoind.spvNode
      val bitcoind = spvNodeConnectedWithBitcoind.bitcoind

      //we need to generate 1 block for bitcoind to consider
      //itself out of IBD. bitcoind will not sendheaders
      //when it believes itself, or it's peer is in IBD
      val gen1F = bitcoind.generate(1)

      //this needs to be called to get our peer to send us headers
      //as they happen with the 'sendheaders' message
      //both our spv node and our bitcoind node _should_ both be at the genesis block (regtest)
      //at this point so no actual syncing is happening
      val initSyncF = gen1F.flatMap { hashes =>
        val syncF = spvNode.sync()
        for {
          _ <- syncF
          _ <- NodeTestUtil.awaitBestHash(hashes.head, spvNode)
        } yield ()
      }

      //start generating a block every 10 seconds with bitcoind
      //this should result in 5 blocks
      val startGenF = initSyncF.map { _ =>
        //generate a block every 5 seconds
        //until we have generated 5 total blocks
        genBlockInterval(bitcoind)
      }

      startGenF.flatMap { _ =>
        //we should expect 5 headers have been announced to us via
        //the send headers message.
        val has6BlocksF = RpcUtil.retryUntilSatisfiedF(
          conditionF =
            () => spvNode.chainApiFromDb().flatMap(_.getBlockCount.map(_ == 6)),
          duration = 250.millis)

        has6BlocksF.map(_ => succeed)
      }
  }

  /** Helper method to generate blocks every interval */
  private def genBlockInterval(bitcoind: BitcoindRpcClient)(
      implicit system: ActorSystem): Unit = {

    var counter = 0
    val desiredBlocks = 5
    val interval = 500.millis

    val genBlock = new Runnable {
      override def run(): Unit = {
        if (counter < desiredBlocks) {
          bitcoind.generate(1)
          counter = counter + 1
        } else {
          //do nothing
        }
      }
    }

    system.scheduler.schedule(2.second, interval, genBlock)
  }
}
