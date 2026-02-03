package org.bitcoins.node.networking.peer

import org.apache.pekko.NotUsed
import org.apache.pekko.stream.OverflowStrategy
import org.apache.pekko.stream.scaladsl.{
  Flow,
  Keep,
  Sink,
  Source,
  SourceQueueWithComplete,
  Tcp
}
import org.apache.pekko.util.ByteString
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.p2p.{NetworkMessage, PingMessage}
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.NodeStreamMessage
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.NodeUnitTest
import org.scalatest.{Assertion, FutureOutcome}

import java.net.InetSocketAddress
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class PeerConnectionTest extends NodeUnitTest {
  case class TestHelper(
      peerConnection: PeerConnection,
      serverBindingF: Future[Tcp.ServerBinding],
      outboundMessagesF: Future[Seq[NetworkMessage]],
      aggregateInboundQueue: SourceQueueWithComplete[NodeStreamMessage])
  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(
      postgresOpt,
      Vector.empty
    )

  override type FixtureParam = BitcoinSAppConfig
  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withBitcoinSAppConfig(test)
  behavior of "PeerConnection"

  it must "relay and consume information over a peer connection" in { param =>
    val test: (ChainAppConfig, NodeAppConfig) => Future[Assertion] = {
      case (chainAppConfig: ChainAppConfig, nodeAppConfig: NodeAppConfig) =>
        val testHelper =
          setupPeerConnection()(using chainAppConfig, nodeAppConfig)
        val pc = testHelper.peerConnection
        val serverBindingF = testHelper.serverBindingF
        val messagesF = testHelper.outboundMessagesF
        for {
          serverBinding <- serverBindingF
          _ <- pc.connect()
          _ <- AsyncUtil.nonBlockingSleep(2.second)
          _ <- pc.disconnect()
          _ <- serverBinding.unbind()
          messages <- messagesF
        } yield {
          assert(messages.nonEmpty)
        }
    }

    test(param.chainConf, param.nodeConf)
  }

  it must "send a lot of outbound messages" in { param =>
    val test: (ChainAppConfig, NodeAppConfig) => Future[Assertion] = {
      case (chainAppConfig: ChainAppConfig, nodeAppConfig: NodeAppConfig) =>
        val testHelper =
          setupPeerConnection()(using chainAppConfig, nodeAppConfig)
        val pc = testHelper.peerConnection
        val serverBindingF = testHelper.serverBindingF
        val messagesF = testHelper.outboundMessagesF
        for {
          serverBinding <- serverBindingF
          _ <- pc.connect()
          pings <- sendAlotOfPings(PeerConnection.outboundBufferSize * 10, pc)
          _ <- AsyncUtil.nonBlockingSleep(2.second)
          _ <- pc.disconnect()
          _ <- serverBinding.unbind()
          messages <- messagesF
        } yield {
          assert(messages.size == pings.size + 1) // +1 for version message
        }
    }
    test(param.chainConf, param.nodeConf)
  }

  private def setupPeerConnection()(implicit
      chainAppConfig: ChainAppConfig,
      nodeAppConfig: NodeAppConfig): TestHelper = {
    val (aggregateInboundQueue, aggregateInboundSource) =
      Source
        .queue[NodeStreamMessage](1024, OverflowStrategy.backpressure)
        .preMaterialize()

    val socket =
      InetSocketAddress.createUnresolved("127.0.0.1", NetworkUtil.randomPort())
    val peer: Peer = Peer(socket, None, None)

    val serverSource
        : Source[Tcp.IncomingConnection, Future[Tcp.ServerBinding]] =
      Tcp().bind(peer.socket.getHostString, peer.port)
    val pc = PeerConnection(peer, aggregateInboundQueue)

    val connSink: Sink[Tcp.IncomingConnection, Future[Seq[NetworkMessage]]] =
      Sink.foldAsync(Seq.empty[NetworkMessage]) {
        case (msgs, incoming: Tcp.IncomingConnection) =>
          val connFlow: Flow[ByteString, NetworkMessage, NotUsed] =
            incoming.flow
              .joinMat(PeerConnection.bidiFlow)(Keep.left)
              .mapConcat(identity)
          val result: Sink[ByteString, Future[Seq[NetworkMessage]]] =
            connFlow.toMat(Sink.seq)(Keep.right)

          Source.maybe
            .runWith(result)
            .map(msgs ++ _)
      }

    val (serverBindingF: Future[Tcp.ServerBinding],
         outboundMessagesF: Future[Seq[NetworkMessage]]) = serverSource
      .toMat(connSink)(Keep.both)
      .run()
    TestHelper(
      peerConnection = pc,
      serverBindingF = serverBindingF,
      outboundMessagesF = outboundMessagesF,
      aggregateInboundQueue = aggregateInboundQueue
    )
  }

  private def sendAlotOfPings(
      numPings: Int,
      pc: PeerConnection): Future[Vector[PingMessage]] = {
    val pings = 0.until(numPings).map { num => PingMessage(UInt64(num)) }
    Future
      .traverse(pings)(ping => pc.sendMsg(ping).map(_ => ping))
      .map(_.toVector)
  }

  def sendAlotOfPings(numPings: Int, queue: SourceQueueWithComplete[ByteString])
      : Future[Vector[PingMessage]] = {
    Source(0.until(numPings))
      .mapAsync(Runtime.getRuntime.availableProcessors()) { num =>
        val p = PingMessage(UInt64(num))
        queue
          .offer(ByteString(p.bytes.toArray))
          .map(_ => p)
      }
      .toMat(Sink.seq)(Keep.right)
      .run()
      .map(_.toVector)
  }
}
