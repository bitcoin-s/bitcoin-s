package org.bitcoins.node

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.p2p.GetHeadersMessage
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.P2PClient.ExpectResponseCommand
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoinds
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
    BitcoindRpcTestUtil.createNodePair().map(p => Vector(p._1, p._2))

  lazy val bitcoinPeersF: Future[Vector[Peer]] = {
    bitcoindsF.flatMap { bitcoinds =>
      val peersF = bitcoinds.map(NodeTestUtil.getBitcoindPeer)
      Future.sequence(peersF)
    }
  }

  override protected def getFreshConfig: BitcoinSAppConfig = {
    BitcoinSTestAppConfig.getMultiPeerNeutrinoWithEmbeddedDbTestConfig(pgUrl)
  }

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoinds

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val torClientF = if (TorUtil.torEnabled) torF else Future.unit

    val outcomeF: Future[Outcome] = for {
      _ <- torClientF
      bitcoinds <- bitcoindsF
      outcome = withUnsyncedNeutrinoNodeConnectedToBitcoinds(test, bitcoinds)(
        system,
        getFreshConfig)
      f <- outcome.toFuture
    } yield f
    new FutureOutcome(outcomeF)
  }

  behavior of "NeutrinoNode"

  it must "switch to different peer and sync if current is unavailable" in {
    nodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoinds.node
      val bitcoinds = nodeConnectedWithBitcoinds.bitcoinds
      val peerManager = node.peerManager
      def peers = peerManager.peers

      for {
        bitcoindPeers <- bitcoinPeersF
        _ <- AsyncUtil.retryUntilSatisfied(peers.size == 2,
                                           maxTries = 30,
                                           interval = 1.second)
        //sync from first bitcoind
        _ = node.updateDataMessageHandler(
          node.getDataMessageHandler.copy(syncPeer = Some(bitcoindPeers(0)))(
            executionContext,
            node.nodeAppConfig,
            node.chainAppConfig))
        expectHeaders = ExpectResponseCommand(
          GetHeadersMessage(node.chainConfig.chain.genesisHash))
        //waiting for response to header query now
        client <- peerManager.peerData(bitcoindPeers(0)).client
        _ = client.actor ! expectHeaders
        nodeUri <- NodeTestUtil.getNodeURIFromBitcoind(bitcoinds(0))
        _ <- bitcoinds(0).disconnectNode(nodeUri)
        //should now sync from bitcoinds(1)
        _ <- NodeTestUtil.awaitAllSync(node, bitcoinds(1))
        newSyncPeer = node.getDataMessageHandler.syncPeer.get
        peer2 = bitcoindPeers(1)
      } yield {
        assert(newSyncPeer == peer2)
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
