package org.bitcoins.node

import org.apache.pekko.stream.OverflowStrategy
import org.apache.pekko.stream.scaladsl.{Source, SourceQueue}
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.node.Peer
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.networking.peer.{PeerConnection, PeerMessageSender}
import org.bitcoins.testkit.util.BitcoinSAsyncTest

import java.net.InetSocketAddress
import java.nio.file.Files

class PeerStackTest extends BitcoinSAsyncTest {

  behavior of "PeerStack"

  val tempDir = Files.createTempDirectory("bitcoin-s")

  private implicit val nodeAppConfig: NodeAppConfig =
    NodeAppConfig(baseDatadir = tempDir, Vector.empty)
  private implicit val chainAppConfig: ChainAppConfig = {
    ChainAppConfig(tempDir, Vector.empty)
  }
  it must "not push the same ip address twice" in {
    val stack = PeerStack()
    val queue: SourceQueue[NodeStreamMessage] = Source
      .queue[NodeStreamMessage](1, OverflowStrategy.backpressure)
      .preMaterialize()
      ._1
    val peer0 =
      Peer(InetSocketAddress.createUnresolved("127.0.0.1", 8333), None, None)
    val peerMessageSender0 = PeerMessageSender(PeerConnection(peer0, queue))
    val pd0: PeerData =
      PersistentPeerData(peer = peer0, peerMessageSender = peerMessageSender0)

    stack.pushAll(Vector(pd0, pd0))
    assert(stack.size == 1)

    val peer1 =
      Peer(InetSocketAddress.createUnresolved("128.0.0.1", 8333), None, None)

    val peerMessageSender1 = PeerMessageSender(PeerConnection(peer1, queue))

    val pd1: PeerData =
      PersistentPeerData(peer = peer1, peerMessageSender = peerMessageSender1)

    stack.pushAll(Vector(pd1, pd1))
    assert(stack.size == 2)
    val popPd1 = stack.pop()
    assert(popPd1 == pd1)
    val popPd0 = stack.pop()
    assert(popPd0 == pd0)
    assert(stack.size == 0)
  }
}
