package org.bitcoins.node.networking.peer

import akka.actor.ActorRef
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.number.Int32
import org.bitcoins.core.p2p.{InetAddress, VerAckMessage, VersionMessage}
import org.bitcoins.node.constant.NodeConstants
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.NodeTestWithCachedBitcoindNewest
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoind
import org.bitcoins.testkit.util.TorUtil
import org.scalatest.{FutureOutcome, Outcome}

import java.net.InetSocketAddress
import scala.concurrent.{Future, Promise}

class PeerMessageReceiverTest extends NodeTestWithCachedBitcoindNewest {

  /** Wallet config with data directory set to user temp directory */
  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getMultiPeerNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val torClientF = if (TorUtil.torEnabled) torF else Future.unit

    val outcomeF: Future[Outcome] = for {
      _ <- torClientF
      bitcoind <- cachedBitcoindWithFundsF
      outcome = withNeutrinoNodeConnectedToBitcoindCached(test, bitcoind)(
        system,
        getFreshConfig)
      f <- outcome.toFuture
    } yield f
    new FutureOutcome(outcomeF)
  }

  behavior of "PeerMessageReceiverTest"

  it must "change a peer message receiver to be disconnected" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoind =>
      val node = nodeConnectedWithBitcoind.node
      val socket = InetSocketAddress.createUnresolved("google.com", 12345)
      val peer = Peer(socket, None, None)
      val client = P2PClient(ActorRef.noSender, peer)
      val clientP = Promise[P2PClient]()
      clientP.success(client)

      val versionMsgP = Promise[VersionMessage]()
      val localhost = java.net.InetAddress.getLocalHost
      val versionMsg = VersionMessage(RegTest,
                                      NodeConstants.userAgent,
                                      Int32.one,
                                      InetAddress(localhost.getAddress),
                                      InetAddress(localhost.getAddress),
                                      false)
      versionMsgP.success(versionMsg)

      val verackMsgP = Promise[VerAckMessage.type]()
      verackMsgP.success(VerAckMessage)

      val normal = PeerMessageReceiverState.Normal(clientConnectP = clientP,
                                                   clientDisconnectP =
                                                     Promise[Unit](),
                                                   versionMsgP = versionMsgP,
                                                   verackMsgP = verackMsgP)

      val peerMsgReceiver =
        PeerMessageReceiver(normal, node, peer)(system, node.nodeAppConfig)

      val newMsgReceiverF = peerMsgReceiver.disconnect()

      newMsgReceiverF.map { newMsgReceiver =>
        assert(
          newMsgReceiver.state
            .isInstanceOf[PeerMessageReceiverState.Disconnected])
        assert(newMsgReceiver.isDisconnected)
      }

  }

  it must "change a peer message receiver to be initializing disconnect" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoind =>
      val node = nodeConnectedWithBitcoind.node
      val socket = InetSocketAddress.createUnresolved("google.com", 12345)
      val peer = Peer(socket, None, None)
      val client = P2PClient(ActorRef.noSender, peer)
      val clientP = Promise[P2PClient]()
      clientP.success(client)

      val versionMsgP = Promise[VersionMessage]()
      val localhost = java.net.InetAddress.getLocalHost
      val versionMsg = VersionMessage(RegTest,
                                      NodeConstants.userAgent,
                                      Int32.one,
                                      InetAddress(localhost.getAddress),
                                      InetAddress(localhost.getAddress),
                                      false)
      versionMsgP.success(versionMsg)

      val verackMsgP = Promise[VerAckMessage.type]()
      verackMsgP.success(VerAckMessage)

      val normal = PeerMessageReceiverState.Normal(clientConnectP = clientP,
                                                   clientDisconnectP =
                                                     Promise[Unit](),
                                                   versionMsgP = versionMsgP,
                                                   verackMsgP = verackMsgP)

      val peerMsgReceiver =
        PeerMessageReceiver(normal, node, peer)(system, node.nodeAppConfig)

      val newMsgReceiver = peerMsgReceiver.initializeDisconnect()

      assert(
        newMsgReceiver.state
          .isInstanceOf[PeerMessageReceiverState.InitializedDisconnect])
      assert(!newMsgReceiver.isDisconnected)

      newMsgReceiver.disconnect().map { disconnectRecv =>
        assert(
          disconnectRecv.state
            .isInstanceOf[PeerMessageReceiverState.InitializedDisconnectDone])
        assert(disconnectRecv.isDisconnected)
        assert(disconnectRecv.state.clientDisconnectP.isCompleted)
      }
  }
}
