package org.bitcoins.node

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.node.models.Peer
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoinds
import org.bitcoins.testkit.node.{NodeTestUtil, NodeUnitTest}
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.tor.CachedTor
import org.bitcoins.testkit.util.{AkkaUtil, TorUtil}
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
        zipped = bitcoinds.zip(bitcoindPeers)
        _ <- AsyncUtil.retryUntilSatisfied(peers.size == 2,
                                           interval = 1.second,
                                           maxTries = 10)
        _ <- node.sync()
        sync1 = zipped.find(_._2 == node.getDataMessageHandler.syncPeer.get).get
        h1 <- sync1._1.getBestHashBlockHeight()
        _ <- sync1._1.stop()
        _ <- AkkaUtil.nonBlockingSleep(3.seconds)
        // generating new blocks from the other bitcoind instance
        other = bitcoinds.filterNot(_ == sync1._1).head
        _ <- other.getNewAddress.flatMap(other.generateToAddress(10, _))
        sync2 = zipped.find(_._2 == node.getDataMessageHandler.syncPeer.get).get
        _ <- NodeTestUtil.awaitAllSync(node, sync2._1)
        h2 <- sync2._1.getBestHashBlockHeight()
        //starting it again for subsequent tests
        _ <- sync1._1.start()
      } yield {
        assert(sync1._2 != sync2._2 && h2 - h1 == 10)
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
