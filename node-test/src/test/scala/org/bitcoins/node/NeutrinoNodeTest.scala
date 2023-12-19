package org.bitcoins.node

import akka.actor.Cancellable
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.models.{CompactFilterDAO, CompactFilterHeaderDAO}
import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.models.{PeerDAO, PeerDb}
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
    BitcoinSTestAppConfig.getMultiPeerNeutrinoWithEmbeddedDbTestConfig(
      () => pgUrl(),
      Vector.empty)
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
      _ <- Future
        .sequence(peers.map(peerManager.isConnected))
        .flatMap(p => assert(p.forall(_ == true)))
      res <- Future
        .sequence(peers.map(peerManager.isInitialized))
        .flatMap(p => assert(p.forall(_ == true)))
    } yield res

    for {
      _ <- connAndInit
      _ <- NodeTestUtil.awaitSyncAndIBD(node, bitcoinds.head)
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
                                      maxTries = 30)
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
            !peerManager
              .getPeerData(p)
              .isDefined)
          .forall(_ == true),
        maxTries = 5,
        interval = 1.second
      )

      for {
        _ <- has2Peers
        _ <- bothOurs
        _ <- allConnected
        _ <- allInitialized
        _ <- Future.sequence(peers.map(peerManager.disconnectPeer))
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
        _ <- Future
          .sequence(peers.map(peerManager.isConnected))
          .flatMap(p => assert(p.forall(_ == true)))
        _ <- NodeTestUtil.awaitSyncAndIBD(node, bitcoind)
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

      //we need to generate 1 block for bitcoind to consider
      //itself out of IBD. bitcoind will not sendheaders
      //when it believes itself, or it's peer is in IBD
      val gen1F = for {
        _ <- NodeTestUtil.awaitSyncAndIBD(node, bitcoind)
        x <- bitcoind.generate(1)
      } yield x

      //this needs to be called to get our peer to send us headers
      //as they happen with the 'sendheaders' message
      //both our spv node and our bitcoind node _should_ both be at the genesis block (regtest)
      //at this point so no actual syncing is happening
      val initSyncF = gen1F.flatMap { _ =>
        for {
          _ <- NodeTestUtil.awaitBestHash(node, bitcoind)
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

  //intended for test fixtures
  it must "sync filters when multiple header messages are sent in succession" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoind.node
      val bitcoind = nodeConnectedWithBitcoind.bitcoinds(0)

      for {
        _ <- NodeTestUtil.awaitSyncAndIBD(node, bitcoind)
        _ <- AkkaUtil.nonBlockingSleep(3.seconds)
        //have to generate the block headers independent of one another
        //rather than just calling generateToAddress(2,junkAddress)
        //because of this bitcoin core bug: https://github.com/bitcoin-s/bitcoin-s/issues/1098
        //hopefully they fix it some day...
        _ <- bitcoind.generateToAddress(1, junkAddress)
        _ <- AsyncUtil.nonBlockingSleep(500.millis)
        _ <- bitcoind.generateToAddress(1, junkAddress)
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind)
      } yield {
        succeed
      }
  }

  it must "start syncing compact filter headers / compact filters when block header is seen" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoinds =>
      //see: https://github.com/bitcoin-s/bitcoin-s/issues/4933
      val node = nodeConnectedWithBitcoind.node
      val bitcoind = nodeConnectedWithBitcoind.bitcoinds(0)

      for {
        _ <- NodeTestUtil.awaitSyncAndIBD(node, bitcoind)
        _ <- node.stop()
        //drop all compact filter headers / filters
        _ <- CompactFilterHeaderDAO()(executionContext, node.chainConfig)
          .deleteAll()
        _ <- CompactFilterDAO()(executionContext, node.chainConfig).deleteAll()
        _ <- bitcoind.generate(1)
        //restart the node
        _ <- node.start()
        //await for us to sync compact filter headers filters
        //the sync process should get kicked off after we see the
        //newly mined block header
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind)
      } yield {
        succeed
      }
  }

  it must "sync block headers that occurred while were syncing compact filters during IBD" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoinds =>
      //see: https://github.com/bitcoin-s/bitcoin-s/issues/5017

      //problem: We are generating blocks with bitcoinds(0)
      //but we are trying to sync these blocks with bitcoinds(1)
      //this results in a race condition between the bitcoin-s node and bitcoinds(0) / bitcoinds(1) syncing with each other
      //some cases, when we try to sync filters / filter headers from bitcoinds(1) we will get this error message saying we cannot find the block
      //2023-05-01T21:46:46Z [net] Failed to find block filter hashes in index: filter_type=basic, start_height=208, stop_hash=303cc906bf99b5370581e7f23285378c18005745882c6112dbbf3e61a82aeddb
      val node = nodeConnectedWithBitcoind.node
      val bitcoind = nodeConnectedWithBitcoind.bitcoinds(0)

      //start syncing node
      val numBlocks = 5
      val genBlocksF = {
        for {
          //generate blocks while sync is ongoing
          _ <- bitcoind.generate(numBlocks)
        } yield {
          ()
        }
      }

      for {
        _ <- genBlocksF
        //wait for sync to complete
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind)
        //generate another block and make sure it syncs it
        _ <- bitcoind.generate(1)
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind)
      } yield {
        succeed
      }
  }

  it must "count peer connections correctly" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoind.node
      val bitcoinds = nodeConnectedWithBitcoind.bitcoinds
      for {
        _ <- AsyncUtil.nonBlockingSleep(1.second)
        initConnectionCount <- node.getConnectionCount
        _ = assert(initConnectionCount == 2)
        bitcoind0 = bitcoinds(0)
        nodeUri0 <- NodeTestUtil.getNodeURIFromBitcoind(bitcoind0)
        peer0 <- NodeTestUtil.getBitcoindPeer(bitcoind0)
        _ <- bitcoind0.disconnectNode(nodeUri0)
        _ <- AsyncUtil.retryUntilSatisfiedF(
          () => node.peerManager.isDisconnected(peer0),
          1.second)
        _ <- AsyncUtil.retryUntilSatisfiedF(() =>
          node.getConnectionCount.map(_ == 1))
      } yield succeed
  }

  it must "start syncing compact filters on startup when block headers / filter headers are synced" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoinds =>
      //https://github.com/bitcoin-s/bitcoin-s/issues/5221
      val node = nodeConnectedWithBitcoind.node
      val bitcoinds = nodeConnectedWithBitcoind.bitcoinds
      val bitcoind0 = bitcoinds(0)
      val blockCountF = bitcoind0.getBlockCount()
      for {
        blockCount <- blockCountF
        _ <- AsyncUtil.retryUntilSatisfiedF(() => {
          for {
            chainApi <- node.chainApiFromDb()
            fhCount <- chainApi.getFilterHeaderCount()
          } yield fhCount == blockCount
        })
        _ <- node.stop()
        _ <- node.start()
        _ <- AsyncUtil.retryUntilSatisfiedF(
          () => {
            for {
              chainApi <- node.chainApiFromDb()
              filterCount <- chainApi.getFilterCount()
            } yield filterCount == blockCount
          },
          1.second)
      } yield succeed
  }

  it must "handle reorgs correctly" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoinds =>
      //https://github.com/bitcoin-s/bitcoin-s/issues/5017
      val node = nodeConnectedWithBitcoind.node
      val bitcoinds = nodeConnectedWithBitcoind.bitcoinds
      val bitcoind0 = bitcoinds(0)
      val bitcoind1 = bitcoinds(1)
      for {
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind0)
        //disconnect bitcoind1 as we don't need it
        nodeUri1 <- NodeTestUtil.getNodeURIFromBitcoind(bitcoind1)
        _ <- bitcoind1.disconnectNode(nodeUri1)
        bestBlockHash0 <- bitcoind0.getBestBlockHash()
        _ <- bitcoind0.invalidateBlock(bestBlockHash0)
        //now generate a block, make sure we sync with them
        _ <- bitcoind0.generate(1)
        _ <- AsyncUtil.nonBlockingSleep(1.second)
        //generate another block to make sure the reorg is complete
        _ <- bitcoind0.generate(1)
        _ <- NodeTestUtil.awaitAllSync(node, bitcoind0)
      } yield succeed
  }
}
