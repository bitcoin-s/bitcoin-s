package org.bitcoins.node

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.p2p.{HeadersMessage}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.fixture.NeutrinoNodeNotConnectedWithBitcoinds
import org.bitcoins.testkit.node.{NodeTestUtil, NodeUnitTest}
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.tor.CachedTor
import org.bitcoins.testkit.util.TorUtil
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

/** Neutrino node tests that require changing the state of bitcoind instance */
class NeutrinoNodeWithUncachedBitcoindTest extends NodeUnitTest with CachedTor {

  lazy val bitcoindsF =
    BitcoindRpcTestUtil
      .createUnconnectedNodePairWithBlocks()
      .map(p => Vector(p._1, p._2))

  lazy val bitcoinPeersF: Future[Vector[Peer]] = {
    bitcoindsF.flatMap { bitcoinds =>
      Future.traverse(bitcoinds)(NodeTestUtil.getBitcoindPeer)
    }
  }

  lazy val invalidHeader = BlockHeader.fromHex(
    s"0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4adae5494dffff7f2003000000"
  )

  override protected def getFreshConfig: BitcoinSAppConfig = {
    BitcoinSTestAppConfig.getMultiPeerNeutrinoWithEmbeddedDbTestConfig(
      () => pgUrl(),
      Vector.empty
    )
  }

  override type FixtureParam = NeutrinoNodeNotConnectedWithBitcoinds

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val torClientF = if (TorUtil.torEnabled) torF else Future.unit

    val outcomeF: Future[Outcome] = for {
      _ <- torClientF
      bitcoinds <- bitcoindsF
      outcome = withUnstartedNeutrinoNodeBitcoinds(test, bitcoinds)(
        using system,
        getFreshConfig
      )
      f <- outcome.toFuture
    } yield f
    new FutureOutcome(outcomeF)
  }

  behavior of "NeutrinoNode"

  it must "switch to different peer and sync if current is unavailable" in {
    nodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoinds.node
      val bitcoinds = nodeConnectedWithBitcoinds.bitcoinds

      for {
        bitcoindPeers <- bitcoinPeersF
        _ <- node.start()
        _ <- NodeTestUtil.awaitConnectionCount(node, bitcoinds.size)
        // sync from first bitcoind
        peer0 = bitcoindPeers(0)
        _ <- node.peerManager.disconnectPeer(peer0)
        _ = logger.debug(
          s"Disconnected $peer0 from node bitcoind(0).p2pPort=${peer0.socket.getPort} bitcoind(1).p2pPort=${bitcoinds(
              1).instance.p2pPort}"
        )
        // old peer we were syncing with that just disconnected us
        _ <- NodeTestUtil.awaitAllSync(node, bitcoinds(1))
      } yield {
        succeed
      }
  }

  it must "have the best header chain post sync from all peers" in {
    nodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoinds.node
      val bitcoinds = nodeConnectedWithBitcoinds.bitcoinds

      for {
        _ <- node.start()
        _ <- NodeTestUtil.awaitConnectionCount(node, bitcoinds.size)
        _ <- bitcoinds(1).generateToAddress(1, junkAddress)
        h1 <- bitcoinds(0).getBestHashBlockHeight()
        h2 <- bitcoinds(1).getBestHashBlockHeight()
        // out of sync by 1 block, h2 ahead
        _ = assert(h2 - h1 == 1)
        _ <- NodeTestUtil.awaitBestHash(node, bitcoinds(1))
      } yield {
        succeed
      }
  }
  // note: now bitcoinds(1) is ahead by 1 block compared to bitcoinds(0)

  it must "re-query in case invalid headers are sent" in {
    nodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoinds.node
      val bitcoinds = nodeConnectedWithBitcoinds.bitcoinds
      for {
        _ <- node.start()
        _ <- NodeTestUtil.awaitConnectionCount(
          node = node,
          expectedConnectionCount = bitcoinds.size
        )
        peers <- bitcoinPeersF
        peer = peers.head
        _ <- NodeTestUtil.awaitAllSync(node, bitcoinds(1))
        // generating 6 blocks will cause bitcoind(1) NOT to gossip them on the p2p network
        // this means we can test our re-query logic by sending an invalid header from bitcoinds(0)
        _ <- bitcoinds(1).generate(6)
        _ <- AsyncUtil.nonBlockingSleep(2.second)
        invalidHeaderMessage = HeadersMessage(headers = Vector(invalidHeader))
        msg = NodeStreamMessage.DataMessageWrapper(invalidHeaderMessage, peer)
        _ <- node.offer(msg)
        bestChain = bitcoinds(1)
        _ <- NodeTestUtil.awaitSync(node, bestChain)
      } yield {
        succeed
      }
  }

  // test for https://github.com/bitcoin-s/bitcoin-s/issues/6073
  it must "disconnect a peer that keeps sending invalid headers" in {
    nodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoinds.node
      val bitcoinds = nodeConnectedWithBitcoinds.bitcoinds
      val peerManager = node.peerManager

      def sendInvalidHeaders(peer: Peer): Future[Unit] = {
        val invalidHeaderMessage =
          HeadersMessage(headers = Vector(invalidHeader))
        val sendFs = {
          val count = 1
            .to(node.nodeAppConfig.maxInvalidResponsesAllowed + 1)
          FutureUtil.sequentially[Int, Unit](count) { _ =>
            val msg =
              NodeStreamMessage.DataMessageWrapper(invalidHeaderMessage, peer)
            node
              .offer(msg)
              // add a delay to not overwhelm queue so other messages can be processed
              .flatMap(_ => AsyncUtil.nonBlockingSleep(100.millis))
          }
        }

        sendFs.map(_ => ())
      }

      for {
        _ <- node.start()
        _ <- NodeTestUtil.awaitConnectionCount(
          node = node,
          expectedConnectionCount = bitcoinds.size
        )
        peers <- bitcoinPeersF
        peer = peers(1)
        _ <- node.peerManager.isConnected(peer).map(assert(_))
        bitcoinds <- bitcoindsF
        bitcoind0 = bitcoinds(0)
        bitcoind1 = bitcoinds(1)
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind1)
        // disconnect bitcoind(0) as its not needed for this test
        peer0 <- NodeTestUtil.getBitcoindPeer(bitcoind0)
        _ <- node.peerManager.disconnectPeer(peer0)
        _ <- AsyncUtil.retryUntilSatisfied(peerManager.peers.size == 1)
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind1)
        _ <- sendInvalidHeaders(peer)
        _ <- AsyncUtil.retryUntilSatisfied(
          !node.peerManager.peers.contains(peer)
        )
      } yield {
        succeed
      }
  }

  override def afterAll(): Unit = {
    val stopF = for {
      bitcoinds <- bitcoindsF
      _ <- BitcoindRpcTestUtil.stopServers(bitcoinds)
    } yield ()
    Await.result(stopF, duration)
    super.afterAll()
  }
}
