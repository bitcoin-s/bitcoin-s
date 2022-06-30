package org.bitcoins.node

import akka.actor.Cancellable
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.models.{Peer, PeerDAO, PeerDb}
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoinds
import org.bitcoins.testkit.node.{NodeTestUtil, NodeTestWithCachedBitcoindPair}
import org.bitcoins.testkit.util.{AkkaUtil, TorUtil}
import org.scalatest.{Assertion, FutureOutcome, Outcome}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class NeutrinoNodeTest extends NodeTestWithCachedBitcoindPair {

  /** Wallet config with data directory set to user temp directory */
  override protected def getFreshConfig: BitcoinSAppConfig = {
    BitcoinSTestAppConfig.getMultiPeerNeutrinoWithEmbeddedDbTestConfig(pgUrl)
  }

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoinds

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val torClientF = if (TorUtil.torEnabled) torF else Future.unit

    val outcomeF: Future[Outcome] = for {
      _ <- torClientF
      bitcoinds <- clientsF
      outcome = withUnsyncedNeutrinoNodeConnectedToBitcoinds(
        test,
        bitcoinds.toVector)(system, getFreshConfig)
      f <- outcome.toFuture
    } yield f
    new FutureOutcome(outcomeF)
  }

  behavior of "NeutrinoNode"

  it must "be able to sync" in { nodeConnectedWithBitcoinds =>
    val node = nodeConnectedWithBitcoinds.node
    val bitcoinds = nodeConnectedWithBitcoinds.bitcoinds
    val peerManager = node.peerManager
    def peers = peerManager.peers

    val connAndInit = for {
      _ <- AsyncUtil
        .retryUntilSatisfied(peers.size == 2, interval = 1.second, maxTries = 5)
      _ <- Future
        .sequence(peers.map(peerManager.isConnected))
        .flatMap(p => assert(p.forall(_ == true)))
      res <- Future
        .sequence(peers.map(peerManager.isConnected))
        .flatMap(p => assert(p.forall(_ == true)))
    } yield res

    val remotesInSync: Future[Assertion] = for {
      h1 <- bitcoinds(0).getBestBlockHash
      h2 <- bitcoinds(1).getBestBlockHash
    } yield assert(h1 == h2)

    for {
      _ <- connAndInit
      _ <- remotesInSync
      _ <- node.sync()
      _ <- NodeTestUtil.awaitAllSync(node, bitcoinds.head)
    } yield {
      succeed
    }
  }

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

      def allDisconn: Future[Unit] = AsyncUtil.retryUntilSatisfied(
        peers
          .map(p =>
            !peerManager.peerData.contains(p) && !peerManager.waitingForDeletion
              .contains(p))
          .forall(_ == true),
        maxTries = 5,
        interval = 1.second
      )

      for {
        _ <- has2Peers
        _ <- bothOurs
        _ <- allConnected
        _ <- allInitialized
        _ = peers.map(peerManager.removePeer)
        _ <- allDisconn
      } yield {
        succeed
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

  it must "receive notification that a block occurred on the p2p network for neutrino" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoind.node
      val bitcoind = nodeConnectedWithBitcoind.bitcoinds(0)
      val peerManager = node.peerManager
      def peers = peerManager.peers

      val assert1F = for {
        _ <- AsyncUtil
          .retryUntilSatisfied(peers.size == 2,
                               interval = 1.second,
                               maxTries = 5)
        _ <- node.sync()
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind)
        _ <- Future
          .sequence(peers.map(peerManager.isConnected))
          .flatMap(p => assert(p.forall(_ == true)))
        res <- Future
          .sequence(peers.map(peerManager.isConnected))
          .flatMap(p => assert(p.forall(_ == true)))
      } yield res

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

      val sync = for {
        _ <- node.sync()
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind)
      } yield ()

      //we need to generate 1 block for bitcoind to consider
      //itself out of IBD. bitcoind will not sendheaders
      //when it believes itself, or it's peer is in IBD
      val gen1F = for {
        _ <- sync
        x <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(1, _))
      } yield x

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
          _ = {
            val isCancelled = cancellable.cancel()
            if (!isCancelled) {
              logger.warn(s"Failed to cancel generating blocks on bitcoind")
            }
          }
          mtp1 <- bitcoind.getMedianTimePast()
          mtp2 <- node.chainApiFromDb().flatMap(_.getMedianTimePast())
        } yield {
          assert(mtp1 == mtp2)
        }
      }
  }

  it must "switch to different peer and sync if current is unavailable" in {
    nodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoinds.node
      val bitcoinds = nodeConnectedWithBitcoinds.bitcoinds
      val peerManager = node.peerManager

      def peers = peerManager.peers

      val bitcoindPeersF =
        Future.sequence(bitcoinds.map(NodeTestUtil.getBitcoindPeer))

      def connAndInit: Future[Assertion] = for {
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

      val remotesInSync: Future[Assertion] = for {
        h1 <- bitcoinds(0).getBestBlockHash
        h2 <- bitcoinds(1).getBestBlockHash
      } yield assert(h1 == h2)

      for {
        bitcoindPeers <- bitcoindPeersF
        _ <- remotesInSync
        zipped = bitcoinds.zip(bitcoindPeers)
        _ <- connAndInit
        _ <- node.sync()
        sync1 = zipped.find(_._2 == node.getDataMessageHandler.syncPeer.get).get
        h1 <- sync1._1.getBestHashBlockHeight()
        _ <- sync1._1.stop()
        // generating new blocks from the other bitcoind instance
        other = bitcoinds.filterNot(_ == sync1._1).head
        _ <- other.getNewAddress.flatMap(other.generateToAddress(10, _))
        _ <- node.sync()
        _ <- AkkaUtil.nonBlockingSleep(3.seconds)
        sync2 = zipped.find(_._2 == node.getDataMessageHandler.syncPeer.get).get
        _ <- NodeTestUtil.awaitSync(node, sync2._1)
        h2 <- sync2._1.getBestHashBlockHeight()
        //these are shared fixtures so, starting it again
        _ <- sync1._1.start()
      } yield {
        assert(sync1._2 != sync2._2 && h2 - h1 == 10)
      }
  }
}
