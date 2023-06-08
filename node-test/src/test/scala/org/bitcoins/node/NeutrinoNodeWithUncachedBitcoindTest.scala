package org.bitcoins.node

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.p2p.{GetHeadersMessage, HeadersMessage, NetworkMessage}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.DataMessageHandlerState.{
  DoneSyncing,
  MisbehavingPeer,
  RemovePeers
}
import org.bitcoins.node.networking.peer.{
  DataMessageHandlerState,
  SendToPeer,
  SyncDataMessageHandlerState
}
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
    BitcoindRpcTestUtil
      .createUnconnectedNodePairWithBlocks()
      .map(p => Vector(p._1, p._2))

  lazy val bitcoinPeersF: Future[Vector[Peer]] = {
    bitcoindsF.flatMap { bitcoinds =>
      val peersF = bitcoinds.map(NodeTestUtil.getBitcoindPeer)
      Future.sequence(peersF)
    }
  }

  lazy val invalidHeader = BlockHeader.fromHex(
    s"0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4adae5494dffff7f2003000000")

  override protected def getFreshConfig: BitcoinSAppConfig = {
    BitcoinSTestAppConfig.getMultiPeerNeutrinoWithEmbeddedDbTestConfig(
      pgUrl,
      Vector.empty)
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
      def peers = node.peerManager.peers

      for {
        bitcoindPeers <- bitcoinPeersF
        _ <- AsyncUtil.retryUntilSatisfied(peers.size == 2,
                                           maxTries = 30,
                                           interval = 1.second)
        //sync from first bitcoind
        peer0 = bitcoindPeers(0)
        _ = node.peerManager.updateDataMessageHandler(
          node.peerManager.getDataMessageHandler.copy(state =
            DataMessageHandlerState.HeaderSync(peer0))(executionContext,
                                                       node.nodeAppConfig,
                                                       node.chainAppConfig))
        networkPayload =
          GetHeadersMessage(node.chainConfig.chain.genesisHash)
        //waiting for response to header query now
        networkMessage = NetworkMessage(networkParam, networkPayload)
        _ <- node.peerManager.offer(SendToPeer(networkMessage, Some(peer0)))
        nodeUri <- NodeTestUtil.getNodeURIFromBitcoind(bitcoinds(0))
        _ <- bitcoinds(0).disconnectNode(nodeUri)
        _ = logger.info(s"Disconnected $nodeUri from bitcoind")
        //old peer we were syncing with that just disconnected us
        oldSyncPeer = node.peerManager.getDataMessageHandler.state match {
          case state: SyncDataMessageHandlerState => state.syncPeer
          case DoneSyncing | _: MisbehavingPeer | _: RemovePeers =>
            sys.error(s"Cannot be in DoneSyncing state while awaiting sync")
        }
        _ <- NodeTestUtil.awaitAllSync(node, bitcoinds(1))
        expectedSyncPeer = bitcoindPeers(1)
      } yield {
        //can't check the peer directly because we reset
        //dataMessageSyncPeer = None when done syncing.
        //awaitAllSync requires us to be done syncing before
        //continuing code execution
        assert(oldSyncPeer != expectedSyncPeer)
      }
  }

  it must "have the best header chain post sync from all peers" in {
    nodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoinds.node
      val bitcoinds = nodeConnectedWithBitcoinds.bitcoinds
      val peerManager = node.peerManager

      def peers = peerManager.peers

      for {
        _ <- AsyncUtil.retryUntilSatisfied(peers.size == 2)
        _ <- bitcoinds(1).generateToAddress(1, junkAddress)
        h1 <- bitcoinds(0).getBestHashBlockHeight()
        h2 <- bitcoinds(1).getBestHashBlockHeight()
        //out of sync by 1 block, h2 ahead
        _ = assert(h2 - h1 == 1)
        _ <- node.sync()
        _ <- NodeTestUtil.awaitBestHash(node, bitcoinds(1))
      } yield {
        succeed
      }
  }
  //note: now bitcoinds(1) is ahead by 1 block compared to bitcoinds(0)

  it must "re-query in case invalid headers are sent" in {
    nodeConnectedWithBitcoinds =>
      //old behavior: When we get done syncing headers from bitcoind(0)
      //we validate those headers against bitcoind(1)
      //after validating those block headers, we sync filter headers from bitcoind(1)

      //new behavior: When we get done syncing headers from bitcoind(0)
      //we validate those headers against bitcoind(1)
      //after validating headers, we trying to sync compact filter headers against bitcoind(0)
      //which is 1 block header behind bitcoind(1) causing us to send an invalid getcfheaders query
      val node = nodeConnectedWithBitcoinds.node
      val bitcoinds = nodeConnectedWithBitcoinds.bitcoinds

      for {
        _ <- AsyncUtil.retryUntilSatisfied(node.peerManager.peers.size == 2)
        peers <- bitcoinPeersF
        peer = peers.head
        _ = node.peerManager.updateDataMessageHandler(
          node.peerManager.getDataMessageHandler.copy(state =
            DataMessageHandlerState.HeaderSync(peer))(executionContext,
                                                      node.nodeConfig,
                                                      node.chainConfig))

        invalidHeaderMessage = HeadersMessage(headers = Vector(invalidHeader))
        _ <- node.peerManager.getDataMessageHandler
          .addToStream(invalidHeaderMessage, peer)
        bestChain = bitcoinds(1)
        _ <- NodeTestUtil.awaitSync(node, bestChain)
      } yield {
        succeed
      }
  }

  it must "must disconnect a peer that keeps sending invalid headers" in {
    nodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoinds.node
      val peerManager = node.peerManager

      def sendInvalidHeaders(peer: Peer): Future[Unit] = {
        val invalidHeaderMessage =
          HeadersMessage(headers = Vector(invalidHeader))
        val sendFs = {
          val count = 1
            .to(node.nodeConfig.maxInvalidResponsesAllowed + 1)
          FutureUtil.sequentially[Int, Unit](count) { _ =>
            node.peerManager.getDataMessageHandler
              .addToStream(invalidHeaderMessage, peer)
              //add a delay to not overwhelm queue so other messages can be processed
              .flatMap(_ => AsyncUtil.nonBlockingSleep(100.millis))
          }
        }

        sendFs.map(_ => ())
      }

      for {
        _ <- AsyncUtil.retryUntilSatisfied(peerManager.peers.size == 2)
        peers <- bitcoinPeersF
        peer = peers(1)
        _ <- node.peerManager.isConnected(peer).map(assert(_))
        bitcoinds <- bitcoindsF

        //disconnect bitcoind(0) as its not needed for this test
        node0Uri <- NodeTestUtil.getNodeURIFromBitcoind(bitcoinds(0))
        _ <- bitcoinds(0).disconnectNode(node0Uri)
        _ <- AsyncUtil.retryUntilSatisfied(peerManager.peers.size == 1)

        _ <- node.sync()

        _ <- NodeTestUtil.awaitAllSync(node, bitcoinds(1))
        _ = node.peerManager.updateDataMessageHandler(
          node.peerManager.getDataMessageHandler.copy(state =
            DataMessageHandlerState.HeaderSync(peer))(executionContext,
                                                      node.nodeConfig,
                                                      node.chainConfig))

        _ <- sendInvalidHeaders(peer)
        _ <- AsyncUtil.retryUntilSatisfied(
          !node.peerManager.peers.contains(peer))
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
