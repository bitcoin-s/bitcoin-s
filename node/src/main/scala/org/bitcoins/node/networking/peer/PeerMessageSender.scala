package org.bitcoins.node.networking.peer

import akka.actor.{ActorRef, ActorSystem}
import akka.io.Tcp.SO.KeepAlive
import akka.stream.scaladsl.{Flow, Keep, Sink, Source, Tcp}
import akka.{NotUsed}
import akka.util.{ByteString, Timeout}
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.p2p._
import org.bitcoins.crypto.{DoubleSha256Digest, HashDigest}
import org.bitcoins.node.P2PLogger
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.P2PClient.ExpectResponseCommand
import scodec.bits.ByteVector

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future, Promise}

case class PeerMessageSender(client: P2PClient)(implicit
    conf: NodeAppConfig,
    system: ActorSystem)
    extends P2PLogger {
  private val socket = client.peer.socket
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

  //want to receive ByteString, parse it into a protocol message it, pass the parsed protocol message downstream and cache unaligned bytes
  val sink: Sink[ByteString, NotUsed] = Flow[ByteString]
    .statefulMap(() => ByteString.empty)(parseHelper, { _: ByteString => None })
    .to(Sink.actorRef(client.actor, (), { case _: Throwable => () }))

  @volatile private[this] var connectionP: Option[Promise[Option[Nothing]]] =
    None

  /** Initiates a connection with the given peer */
  def connect(): Unit = {
    connectionP match {
      case Some(_) =>
        logger.warn(s"Connected already.")
        ()
      case None =>
        val p: Promise[Option[Nothing]] =
          connection.toMat(sink)(Keep.left).runWith(Source.maybe)
        connectionP = Some(p)
        ()
    }

    //client.actor ! P2PClient.ConnectCommand

  }

  def reconnect(): Unit = {
    client.actor ! P2PClient.ReconnectCommand
  }

  def isConnected()(implicit ec: ExecutionContext): Future[Boolean] = {
    Future.successful(connectionP.isDefined)
  }

  def isInitialized()(implicit ec: ExecutionContext): Future[Boolean] = {
    client.isInitialized()
  }

  def isDisconnected()(implicit ec: ExecutionContext): Future[Boolean] = {
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
    val networkMsg = NetworkMessage(conf.network, msg)
    sendMsg(networkMsg)
  }

  private[node] def sendMsg(msg: NetworkMessage): Future[Unit] = {
    logger.debug(s"Sending msg=${msg.header.commandName} to peer=${socket}")
    val wrap = msg match {
      case _: ExpectsResponse => ExpectResponseCommand(msg.payload)
      case _                  => msg
    }
    client.actor ! wrap
    Future.unit
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
