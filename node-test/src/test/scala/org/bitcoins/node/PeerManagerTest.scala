package org.bitcoins.node

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.models.{Peer, PeerDAO, PeerDb}
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoinds
import org.bitcoins.testkit.node.{NodeTestUtil, NodeTestWithCachedBitcoindPair}
import org.bitcoins.testkit.util.AkkaUtil
import org.scalatest.{Assertion, FutureOutcome, Outcome}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class PeerManagerTest extends NodeTestWithCachedBitcoindPair {

  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getMultiPeerNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoinds

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val outcomeF: Future[Outcome] = for {
      bitcoinds <- clientsF
      outcome = withUnsyncedNeutrinoNodeConnectedToBitcoinds(
        test,
        bitcoinds.toVector)(system, getFreshConfig)
      f <- outcome.toFuture
    } yield f
    new FutureOutcome(outcomeF)
  }

  behavior of "PeerManager"

  it must "be able to connect, initialize and then disconnect from all peers" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoind.node
      def peerManager = node.peerManager
      def peers = peerManager.peers
      val ourPeersF: Future[Vector[Peer]] = Future.sequence(
        nodeConnectedWithBitcoind.bitcoinds.map(NodeTestUtil.getBitcoindPeer))

      def has2Peers: Future[Unit] =
        AsyncUtil.retryUntilSatisfied(peers.size == 2,
                                      interval = 1.second,
                                      maxTries = 5)
      def bothOurs: Future[Assertion] = ourPeersF.map { ours =>
        assert(ours.map(peers.contains(_)).forall(_ == true))
      }
      def allConnected: Future[Assertion] = for {
        conns <- Future.sequence(peers.map(peerManager.isConnected))
      } yield assert(conns.forall(_ == true))
      def allInitialized: Future[Assertion] = for {
        inits <- Future.sequence(peers.map(peerManager.isInitialized))
      } yield assert(inits.forall(_ == true))

      def allDisconn: Future[Assertion] =
        AkkaUtil.nonBlockingSleep(duration = 5.seconds).map { _ =>
          val disconnected = peers.map(p =>
            !peerManager.peerData.contains(p) && !peerManager.waitingForDeletion
              .contains(p))
          assert(disconnected.forall(_ => true))
        }

      for {
        _ <- has2Peers
        _ <- bothOurs
        _ <- allConnected
        _ <- allInitialized
        _ = peers.map(peerManager.removePeer)
        res <- allDisconn
      } yield {
        res
      }
  }

  it must "store peers after successful initialization" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoind.node
      val peerManager = node.peerManager
      def peers = peerManager.peers

      //is the database peer same as Peer
      def isSame(peerDb: PeerDb, peer: Peer): Boolean = {
        val dbSocket =
          NetworkUtil.parseInetSocketAddress(peerDb.address, peerDb.port)

        val hostMatch: Boolean = {
          if (dbSocket.getHostString == peer.socket.getHostString) true
          else {
            //checking if both are localhost
            //a bit hacky way but resolution of localhost to address cannot be done so as to allow for tor
            //addresses too
            val localhost = Vector("localhost", "127.0.0.1")
            localhost.contains(dbSocket.getHostString) && localhost.contains(
              peer.socket.getHostString)
          }
        }

        hostMatch && dbSocket.getPort == peer.socket.getPort
      }

      //assert connected to 2 peers and both initialised and connected
      val assertConnAndInit = for {
        _ <- AsyncUtil
          .retryUntilSatisfied(peers.size == 2,
                               interval = 1.second,
                               maxTries = 5)
        _ <- Future
          .sequence(peers.map(peerManager.isConnected))
          .flatMap(p => assert(p.forall(_ == true)))
        res <- Future
          .sequence(peers.map(peerManager.isConnected))
          .flatMap(p => assert(p.forall(_ == true)))
      } yield res

      for {
        _ <- assertConnAndInit
        ourPeers <- Future.sequence(
          nodeConnectedWithBitcoind.bitcoinds.map(NodeTestUtil.getBitcoindPeer))
        peerDbs <- PeerDAO()(executionContext, node.nodeAppConfig).findAll()
      } yield {

        val allInDb = ourPeers.forall { p =>
          peerDbs.exists(isSame(_, p))
        }
        assert(allInDb)
      }
  }

  ignore must "remove peer if it disconnects and fails to reconnect" in { _ =>
    assert(true)
  }
}
