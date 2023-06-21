package org.bitcoins.node.networking.peer

import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem}
import akka.io.Tcp.SO.KeepAlive
import akka.stream.scaladsl.BidiFlow
import akka.stream.scaladsl.{
  Flow,
  Keep,
  MergeHub,
  RunnableGraph,
  Sink,
  Source,
  Tcp
}
import akka.util.{ByteString, Timeout}
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.number.Int32
import org.bitcoins.core.p2p._
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.{DoubleSha256Digest, HashDigest}
import org.bitcoins.node.P2PLogger
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.constant.NodeConstants
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.PeerMessageReceiver.NetworkMessageReceived
import org.bitcoins.node.util.PeerMessageSenderApi
import scodec.bits.ByteVector

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}

case class PeerMessageSender(
    client: P2PClient,
    initPeerMessageRecv: PeerMessageReceiver,
    peerMessageSenderApi: PeerMessageSenderApi)(implicit
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig,
    system: ActorSystem)
    extends P2PLogger {
  import system.dispatcher

  private val peer = client.peer
  private val socket = peer.socket

  implicit private val timeout: Timeout = Timeout(30.seconds)

  private val options = Vector(KeepAlive(true))

  private lazy val connection: Flow[
    ByteString,
    ByteString,
    Future[Tcp.OutgoingConnection]] = {
    Tcp(system).outgoingConnection(client.peer.socket, options = options)
  }

  private def parseHelper(
      unalignedBytes: ByteString,
      byteVec: ByteString): (ByteString, Vector[NetworkMessage]) = {
    val bytes: ByteVector = ByteVector(unalignedBytes ++ byteVec)
    logger.trace(s"Bytes for message parsing: ${bytes.toHex}")
    val (messages, newUnalignedBytes) =
      P2PClient.parseIndividualMessages(bytes)
    (ByteString.fromArray(newUnalignedBytes.toArray), messages)
  }

  private val chainApi = ChainHandler.fromDatabase()

  private val versionMsgF: Future[NetworkMessage] = {
    chainApi.getBestHashBlockHeight().map { height =>
      val localhost = java.net.InetAddress.getLocalHost
      val versionMsg =
        VersionMessage(nodeAppConfig.network,
                       NodeConstants.userAgent,
                       Int32(height),
                       InetAddress(localhost.getAddress),
                       InetAddress(localhost.getAddress),
                       nodeAppConfig.relay)
      NetworkMessage(nodeAppConfig.network, versionMsg)
    }
  }

  private val parseToNetworkMsgFlow: Flow[
    ByteString,
    Vector[NetworkMessage],
    NotUsed] = {
    Flow[ByteString]
      .statefulMap(() => ByteString.empty)(parseHelper,
                                           { _: ByteString => None })
      .log("parseToNetworkMsgFlow",
           { case msgs: Vector[NetworkMessage] =>
             msgs
               .map(msg =>
                 s"received msg=${msg.payload.commandName} from peer=$peer")
               .mkString("\n")
           })
  }

  private val writeNetworkMsgFlow: Flow[NetworkMessage, ByteString, NotUsed] = {
    Flow[NetworkMessage].map(msg => ByteString(msg.bytes.toArray))
  }

  private val bidiFlow: BidiFlow[
    ByteString,
    Vector[NetworkMessage],
    NetworkMessage,
    ByteString,
    NotUsed] = {
    BidiFlow.fromFlows(parseToNetworkMsgFlow, writeNetworkMsgFlow)
  }

  private val mergeHubSource: Source[
    NetworkMessage,
    Sink[NetworkMessage, NotUsed]] =
    MergeHub
      .source[NetworkMessage](16)
      .log("mergehub",
           { case msg: NetworkMessage =>
             s"sending msg=${msg.payload.commandName} to peer=$peer"
           })

  private val connectionFlow: Flow[
    NetworkMessage,
    Vector[NetworkMessage],
    Future[Tcp.OutgoingConnection]] =
    connection.joinMat(bidiFlow)(Keep.left)

  private def connectionGraph(
      handleNetworkMsgSink: Sink[
        Vector[NetworkMessage],
        NotUsed]): RunnableGraph[
    (Sink[NetworkMessage, NotUsed], Future[Tcp.OutgoingConnection])] = {
    mergeHubSource
      .viaMat(connectionFlow)(Keep.both)
      .toMat(handleNetworkMsgSink)(Keep.left)
  }

  @volatile private[this] var connectionP: Option[
    Promise[Option[NetworkMessage]]] =
    None

  @volatile private[this] var connectionSinkOpt: Option[
    Sink[NetworkMessage, NotUsed]] = None

  /** Initiates a connection with the given peer */
  def connect(): Future[Unit] = {
    connectionP match {
      case Some(_) =>
        logger.warn(s"Connected already to peer=${client.peer}")
        Future.unit
      case None =>
        logger.info(s"Attempting to connect to peer=${client.peer}")

        val initializing =
          initPeerMessageRecv.connect(client, peerMessageSenderApi)

        val handleNetworkMsgSink: Sink[Vector[NetworkMessage], NotUsed] = {
          Flow[Vector[NetworkMessage]]
            .foldAsync(initializing) { case (peerMsgRecv, msgs) =>
              FutureUtil.foldLeftAsync(peerMsgRecv, msgs) { case (p, msg) =>
                p.handleNetworkMessageReceived(
                  networkMsgRecv = NetworkMessageReceived(msg, client),
                  peerMessageSenderApi = peerMessageSenderApi)
              }
            }
            .to(Sink.ignore)
        }

        val (mergeHubSink: Sink[NetworkMessage, NotUsed],
             outgoingConnectionF: Future[Tcp.OutgoingConnection]) = {
          connectionGraph(handleNetworkMsgSink).run()
        }

        connectionSinkOpt = Some(mergeHubSink)

        val resultF: Future[NotUsed] = for {
          _ <- outgoingConnectionF
          versionMsg <- versionMsgF
        } yield Source.single(versionMsg).runWith(mergeHubSink)

        val p: Promise[Option[NetworkMessage]] =
          Source.maybe[NetworkMessage].toMat(mergeHubSink)(Keep.left).run()
        connectionP = Some(p)

        resultF.map(_ => ())
    }
  }

  def reconnect(): Unit = {
    client.actor ! P2PClient.ReconnectCommand
  }

  def isConnected(): Future[Boolean] = {
    Future.successful(connectionP.isDefined)
  }

  def isInitialized(): Future[Boolean] = {
    client.isInitialized()
  }

  def isDisconnected(): Future[Boolean] = {
    isConnected().map(!_)
  }

  /** Disconnects the given peer */
  def disconnect(): Future[Unit] = {
    connectionP match {
      case Some(p) =>
        p.success(None)
        logger.info(s"Disconnecting peer at socket=${socket}")
        connectionP = None
        Future.unit
      case None =>
        val err =
          s"Cannot disconnect client that is not connected to socket=${socket}!"
        logger.warn(err)
        Future.unit
    }
  }

  def sendFilterClearMessage(): Future[Unit] = {
    sendMsg(FilterClearMessage)
  }

  def sendFilterAddMessage(hash: HashDigest): Future[Unit] = {
    val message = FilterAddMessage.fromHash(hash)
    logger.trace(s"Sending filteradd=$message to peer=${client.peer}")
    sendMsg(message)
  }

  def sendFilterLoadMessage(bloom: BloomFilter): Future[Unit] = {
    val message = FilterLoadMessage(bloom)
    logger.trace(s"Sending filterload=$message to peer=${client.peer}")
    sendMsg(message)
  }

  def sendGetCompactFilterCheckPointMessage(
      stopHash: DoubleSha256Digest): Future[Unit] = {
    val message = GetCompactFilterCheckPointMessage(stopHash)
    logger.debug(s"Sending getcfcheckpt=$message to peer ${client.peer}")
    sendMsg(message)
  }

  private[node] def sendMsg(msg: NetworkPayload): Future[Unit] = {
    //version or verack messages are the only messages that
    //can be sent before we are fully initialized
    //as they are needed to complete our handshake with our peer
    val networkMsg = NetworkMessage(nodeAppConfig.network, msg)
    sendMsg(networkMsg)
  }

  private[node] def sendMsg(msg: NetworkMessage): Future[Unit] = {
    logger.debug(s"Sending msg=${msg.header.commandName} to peer=${socket}")
    /*    val wrap = msg match {
      case _: ExpectsResponse => ExpectResponseCommand(msg.payload)
      case _                  => msg
    }*/
    connectionSinkOpt match {
      case Some(connectionSink) =>
        val sendMsgF = Future {
          Source.single(msg).to(connectionSink).run()
        }.map(_ => ())

        sendMsgF
      case None =>
        val exn = new RuntimeException(
          s"Could not send msg=${msg.payload.commandName} because we do not have an active connection to peer=${peer}")
        Future.failed(exn)
    }
  }
}

object PeerMessageSender {

  sealed abstract class PeerMessageHandlerMsg

  /** For when we are done with exchanging version and verack messages
    * This means we can send normal p2p messages now
    */
  case object HandshakeFinished extends PeerMessageHandlerMsg

  /** Accumulators network messages while we are doing a handshake with our peer
    * and caches a peer handler actor so we can send a [[HandshakeFinished]]
    * message back to the actor when we are fully connected
    */
  case class MessageAccumulator(
      networkMsgs: Vector[(ActorRef, NetworkMessage)],
      peerHandler: ActorRef)

}
