package org.bitcoins.node.networking.peer

import akka.actor.ActorRef
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.number.Int32
import org.bitcoins.core.p2p.{InetAddress, VerAckMessage, VersionMessage}
import org.bitcoins.node.constant.NodeConstants
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.{P2PClient, P2PClientCallbacks}
import org.bitcoins.testkit.util.BitcoinSAsyncTest

import java.net.InetSocketAddress
import scala.concurrent.Promise

class PeerMessageReceiverTest extends BitcoinSAsyncTest {

  behavior of "PeerMessageReceiverTest"

  it must "change a peer message receiver to be disconnected" in {
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

    val newMsgReceiverStateF = normal.disconnect(peer, P2PClientCallbacks.empty)

    newMsgReceiverStateF.map { newMsgReceiverState =>
      assert(
        newMsgReceiverState
          .isInstanceOf[PeerMessageReceiverState.Disconnected])
      assert(newMsgReceiverState.isDisconnected)
    }

  }

  it must "change a peer message receiver to be initializing disconnect" in {
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

    val newMsgReceiverState = normal.initializeDisconnect(peer)

    assert(
      newMsgReceiverState
        .isInstanceOf[PeerMessageReceiverState.InitializedDisconnect])
    assert(!newMsgReceiverState.isDisconnected)

    newMsgReceiverState.disconnect(peer, P2PClientCallbacks.empty).map {
      disconnectRecv =>
        assert(
          disconnectRecv
            .isInstanceOf[PeerMessageReceiverState.InitializedDisconnectDone])
        assert(disconnectRecv.isDisconnected)
        assert(disconnectRecv.clientDisconnectP.isCompleted)
    }
  }
}
