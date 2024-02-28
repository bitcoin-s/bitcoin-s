package org.bitcoins.node

import org.bitcoins.node.models.{PeerDAO, PeerDAOHelper}
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoind
import org.bitcoins.testkit.node.{
  NodeTestUtil,
  NodeTestWithCachedBitcoindNewest
}
import org.bitcoins.testkit.util.TorUtil
import org.scalatest.{FutureOutcome, Outcome}

import java.time.Instant
import scala.concurrent.Future

class PeerManagerTest extends NodeTestWithCachedBitcoindNewest {

  /** Wallet config with data directory set to user temp directory */
  override protected def getFreshConfig: BitcoinSAppConfig = {
    BitcoinSTestAppConfig.getMultiPeerNeutrinoWithEmbeddedDbTestConfig(
      () => pgUrl(),
      Vector.empty)
  }

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val torClientF = if (TorUtil.torEnabled) torF else Future.unit

    val outcomeF: Future[Outcome] = for {
      _ <- torClientF
      bitcoind <- cachedBitcoindWithFundsF
      outcome = withNeutrinoNodeUnstarted(test, bitcoind)(system,
                                                          getFreshConfig)
      f <- outcome.toFuture
    } yield f
    new FutureOutcome(outcomeF)
  }

  it must "add the default peer to the peer manager" in {
    nodeConnectedWithBitcoind =>
      val node = nodeConnectedWithBitcoind.node
      val bitcoind = nodeConnectedWithBitcoind.bitcoind
      val peerF = NodeTestUtil.getBitcoindPeer(bitcoind)

      for {
        _ <- node.start()
        peer <- peerF
        peerManager = node.peerManager
        _ <- NodeTestUtil.awaitConnectionCount(node = node,
                                               expectedConnectionCount = 1)
      } yield {
        assert(peerManager.peers.exists(_ == peer))
        assert(
          peerManager.paramPeers.nonEmpty
        ) //make sure we had a peer passed as a param
      }
  }

  it must "connect a peer that PeerFinder doesn't know about" in {
    nodeConnectedWithBitcoind =>
      val node = nodeConnectedWithBitcoind.node
      val bitcoind = nodeConnectedWithBitcoind.bitcoind
      val peerF = NodeTestUtil.getBitcoindPeer(bitcoind)

      for {
        _ <- node.start()
        peer <- peerF
        peerManager = node.peerManager

        _ <- NodeTestUtil.awaitSyncAndIBD(node = node, bitcoind = bitcoind)
        //disconnect
        address <- peerManager
          .getPeerData(peer)
          .get
          .peerConnection
          .getLocalAddress
          .map(_.get)
        nodeUri <- NodeTestUtil.getNodeURIFromBitcoind(bitcoind, address)
        _ <- bitcoind.disconnectNode(nodeUri)
        _ <- NodeTestUtil.awaitConnectionCount(node, 0)
        _ <- node.peerManager.connectPeer(peer)
        _ <- NodeTestUtil.awaitConnectionCount(node, 1)
      } yield {
        succeed
      }
  }

  it must "update last time a peer was seen on disconnect" in {
    nodeConnectedWithBitcoind =>
      val node = nodeConnectedWithBitcoind.node
      val bitcoind = nodeConnectedWithBitcoind.bitcoind
      val peerF = NodeTestUtil.getBitcoindPeer(bitcoind)

      for {
        _ <- node.start()
        peer <- peerF
        _ <- NodeTestUtil.awaitSyncAndIBD(node = node, bitcoind = bitcoind)
        //disconnect
        timestamp = Instant.now()
        address <- node.peerManager
          .getPeerData(peer)
          .get
          .peerConnection
          .getLocalAddress
          .map(_.get)
        uri <- NodeTestUtil.getNodeURIFromBitcoind(bitcoind, address)
        _ <- bitcoind.disconnectNode(uri)
        _ <- NodeTestUtil.awaitConnectionCount(node, 0)
        addrBytes = PeerDAOHelper.getAddrBytes(peer)
        peerDb <- PeerDAO()(node.nodeConfig, system.dispatcher)
          .read((addrBytes, peer.port))
          .map(_.get)
      } yield {
        assert(timestamp.isBefore(peerDb.lastSeen))
      }
  }

  it must "determine if filters are out of sync" in { _ =>
    val blockCount = 804934
    val oldFilterHeaderCount = 804934
    val currentFilterHeaderCount = 804934
    val oldFilterCount = 771760
    val currentFilterCount = 771760

    val isOutOfSync = PeerManager.isFiltersOutOfSync(
      blockCount = blockCount,
      oldFilterHeaderCount = oldFilterHeaderCount,
      currentFilterHeaderCount = currentFilterHeaderCount,
      oldFilterCount = oldFilterCount,
      currentFilterCount = currentFilterCount
    )

    assert(isOutOfSync)

  }
}
