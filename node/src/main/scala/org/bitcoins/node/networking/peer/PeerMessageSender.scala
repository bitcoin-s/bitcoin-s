package org.bitcoins.node.networking.peer

import akka.actor.ActorRef
import akka.io.Tcp
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.p2p.NetworkMessage
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.p2p._
import org.bitcoins.node.networking.Client

class PeerMessageSender(client: Client)(implicit np: NetworkParameters)
    extends BitcoinSLogger {
  private val socket = client.peer.socket

  /** Initiates a connection with the given peer */
  def connect(): Unit = {
    logger.info(s"Attempting to connect to peer=$socket")
    (client.actor ! Tcp.Connect(socket))
  }

  /** Disconnects the given peer */
  def disconnect(): Unit = {
    logger.info(s"Disconnecting peer at socket=${socket}")
    (client.actor ! Tcp.Close)
  }

  /** Sends a [[org.bitcoins.node.messages.VersionMessage VersionMessage]] to our peer */
  def sendVersionMessage(): Unit = {
    val versionMsg = VersionMessage(client.peer.socket, np)
    logger.trace(s"Sending versionMsg=$versionMsg to peer=${client.peer}")
    sendMsg(versionMsg)
  }

  def sendVerackMessage(): Unit = {
    val verackMsg = VerAckMessage
    sendMsg(verackMsg)
  }

  /** Responds to a ping message */
  def sendPong(ping: PingMessage): Unit = {
    val pong = PongMessage(ping.nonce)
    logger.trace(s"Sending pong=$pong to peer=${client.peer}")
    sendMsg(pong)
  }

  def sendGetHeadersMessage(lastHash: DoubleSha256Digest): Unit = {
    val headersMsg = GetHeadersMessage(lastHash)
    logger.trace(s"Sending getheaders=$headersMsg to peer=${client.peer}")
    sendMsg(headersMsg)
  }

  def sendHeadersMessage(): Unit = {
    val sendHeadersMsg = SendHeadersMessage
    sendMsg(sendHeadersMsg)
  }

  private[node] def sendMsg(msg: NetworkPayload): Unit = {
    logger.debug(s"Sending msg=${msg.commandName} to peer=${socket}")
    val newtworkMsg = NetworkMessage(np, msg)
    client.actor ! newtworkMsg
  }
}

object PeerMessageSender {

  private case class PeerMessageSenderImpl(client: Client)(
      implicit np: NetworkParameters)
      extends PeerMessageSender(client)(np)

  sealed abstract class PeerMessageHandlerMsg

  /**
    * For when we are done with exchanging version and verack messages
    * This means we can send normal p2p messages now
    */
  case object HandshakeFinished extends PeerMessageHandlerMsg

  case class SendToPeer(msg: NetworkMessage) extends PeerMessageHandlerMsg

  /** Accumulators network messages while we are doing a handshake with our peer
    * and caches a peer handler actor so we can send a [[HandshakeFinished]]
    * message back to the actor when we are fully connected
    */
  case class MessageAccumulator(
      networkMsgs: Vector[(ActorRef, NetworkMessage)],
      peerHandler: ActorRef)

  def apply(client: Client, np: NetworkParameters): PeerMessageSender = {
    PeerMessageSenderImpl(client)(np)
  }
}
