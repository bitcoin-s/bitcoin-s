package org.bitcoins.node

import akka.actor.Cancellable
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoinds
import org.bitcoins.testkit.node.{NodeTestUtil, NodeTestWithCachedBitcoindPair}
import org.bitcoins.testkit.util.TorUtil
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

class NeutrinoNodeTest extends NodeTestWithCachedBitcoindPair {

  /** Wallet config with data directory set to user temp directory */
  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoinds

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val torClientF = if (TorUtil.torEnabled) torF else Future.unit

    val outcomeF: Future[Outcome] = for {
      _ <- torClientF
      bitcoinds <- clientsF
      outcome = withNeutrinoNodeConnectedToBitcoinds(test, bitcoinds.toVector)(
        system,
        getFreshConfig)
      f <- outcome.toFuture
    } yield f
    new FutureOutcome(outcomeF)
  }

  behavior of "NeutrinoNode"

  it must "be able to connect,initialize and then disconnect from all peers" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoinds =>
      //checking all peers are connected
      val node = nodeConnectedWithBitcoind.node
      val connFs = node.peers.indices.map(node.isConnected)
      val connF = Future.sequence(connFs).map(_.forall(_ == true))
      val connAssertion = connF.map(assert(_))

      //checking all peers are initialized
      val isInitializedFs = node.peers.indices.map(node.isInitialized)
      val isInitializedF = for {
        _ <- connAssertion
        f <- Future.sequence(isInitializedFs).map(_.forall(_ == true))
      } yield f

      val initAssertion = isInitializedF.map(assert(_))

      val disconnF = for {
        _ <- initAssertion
        _ <- node.stop()
        f <- isAllDisconnectedF(node)
      } yield f
      disconnF.map(assert(_))
  }

  it must "receive notification that a block occurred on the p2p network for neutrino" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoind.node

      val bitcoind = nodeConnectedWithBitcoind.bitcoinds(0)

      val assert1F = for {
        _ <- node.isConnected(0).map(assert(_))
        a2 <- node.isInitialized(0).map(assert(_))
      } yield a2

      val hashF: Future[DoubleSha256DigestBE] = bitcoind.getNewAddress
        .flatMap(bitcoind.generateToAddress(1, _))
        .map(_.head)

      val syncF = for {
        _ <- assert1F
        _ <- hashF
      } yield ()

      syncF.flatMap { _ =>
        NodeTestUtil
          .awaitSync(node, bitcoind)
          .map(_ => succeed)
      }
  }

  it must "stay in sync with a bitcoind instance for neutrino" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoind.node
      val bitcoind = nodeConnectedWithBitcoind.bitcoinds(0)

      //we need to generate 1 block for bitcoind to consider
      //itself out of IBD. bitcoind will not sendheaders
      //when it believes itself, or it's peer is in IBD
      val gen1F =
        bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(1, _))

      //this needs to be called to get our peer to send us headers
      //as they happen with the 'sendheaders' message
      //both our spv node and our bitcoind node _should_ both be at the genesis block (regtest)
      //at this point so no actual syncing is happening
      val initSyncF = gen1F.flatMap { hashes =>
        for {
          _ <- NodeTestUtil.awaitBestHash(hashes.head, node)
        } yield ()
      }

      //start generating a block every 10 seconds with bitcoind
      //this should result in 5 blocks
      val startGenF: Future[Cancellable] = initSyncF.map { _ =>
        //generate a block every 5 seconds
        //until we have generated 5 total blocks
        genBlockInterval(bitcoind)
      }

      startGenF.flatMap { cancellable =>
        //we should expect 5 headers have been announced to us via
        //the send headers message.
        for {
          _ <- NodeTestUtil.awaitSync(node, bitcoind)
        } yield {
          val isCancelled = cancellable.cancel()
          if (!isCancelled) {
            logger.warn(s"Failed to cancel generating blocks on bitcoind")
          }
          succeed
        }
      }
  }

  //checking all peers can be disconnected
  private def isAllDisconnectedF(node: Node): Future[Boolean] = {
    val disconnFs = node.peers.indices.map(node.isDisconnected)
    val res = Future.sequence(disconnFs).map(_.forall(_ == true))
    res
  }
}
