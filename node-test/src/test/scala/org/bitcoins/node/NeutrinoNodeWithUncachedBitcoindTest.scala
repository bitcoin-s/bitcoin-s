package org.bitcoins.node

import com.typesafe.config.ConfigFactory
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.p2p.{GetHeadersMessage, HeadersMessage}
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
    BitcoindRpcTestUtil
      .createUnconnectedNodePairWithBlocks()
      .map(p => Vector(p._1, p._2))

  lazy val bitcoinPeersF: Future[Vector[Peer]] = {
    bitcoindsF.flatMap { bitcoinds =>
      val peersF = bitcoinds.map(NodeTestUtil.getBitcoindPeer)
      Future.sequence(peersF)
    }
  }

  override protected def getFreshConfig: BitcoinSAppConfig = {
    val configs = ConfigFactory.parseString("""
                                              | peer-discovery-timeout = 5s
                                              |""".stripMargin)
    BitcoinSTestAppConfig.getMultiPeerNeutrinoWithEmbeddedDbTestConfig(pgUrl,
                                                                       configs)
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
        _ <- NodeTestUtil.awaitSync(node, bitcoinds(1))
      } yield {
        succeed
      }
  }
  //note: now bitcoinds(1) is ahead by 1 block compared to bitcoinds(0)

  it must "re-query in case invalid headers are sent" in {
    nodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoinds.node
      val bitcoinds = nodeConnectedWithBitcoinds.bitcoinds

      for {
        _ <- AsyncUtil.retryUntilSatisfied(node.peerManager.peers.size == 2)
        peers <- bitcoinPeersF
        peer = peers.head
        _ = node.updateDataMessageHandler(
          node.getDataMessageHandler.copy(syncPeer = Some(peer))(
            executionContext,
            node.nodeConfig,
            node.chainConfig))

        invalidHeader = node.chainAppConfig.chain.genesisBlock.blockHeader
        invalidHeaderMessage = HeadersMessage(headers = Vector(invalidHeader))
        sender <- node.peerManager.peerData(peer).peerMessageSender
        _ <- node.getDataMessageHandler.addToStream(invalidHeaderMessage,
                                                    sender,
                                                    peer)
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
        val invalidHeader = node.chainAppConfig.chain.genesisBlock.blockHeader
        val invalidHeaderMessage =
          HeadersMessage(headers = Vector(invalidHeader))
        val senderF = node.peerManager.peerData(peer).peerMessageSender

        for {
          sender <- senderF
          sendFs = 1
            .to(node.nodeConfig.maxInvalidResponsesAllowed + 1)
            .map(_ =>
              node.getDataMessageHandler.addToStream(invalidHeaderMessage,
                                                     sender,
                                                     peer))
          _ <- Future.sequence(sendFs)
        } yield ()
      }

      for {
        _ <- AsyncUtil.retryUntilSatisfied(peerManager.peers.size == 2)
        peers <- bitcoinPeersF
        peer = peers(0)
        _ <- node.peerManager.isConnected(peer).map(assert(_))
        _ = node.updateDataMessageHandler(
          node.getDataMessageHandler.copy(syncPeer = Some(peer))(
            executionContext,
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
