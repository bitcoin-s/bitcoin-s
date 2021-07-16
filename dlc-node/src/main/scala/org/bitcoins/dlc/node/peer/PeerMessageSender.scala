package org.bitcoins.dlc.node.peer

import akka.io.Tcp
import akka.util.Timeout
import grizzled.slf4j.Logging
import org.bitcoins.core.protocol.dlc.models.DLCMessage._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.dlc.node.P2PClient
import scodec.bits.ByteVector

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

case class PeerMessageSender(client: P2PClient) extends Logging {
  private val socket = client.peer.socket
  implicit private val timeout: Timeout = Timeout(30.seconds)

  /** Initiates a connection with the given peer */
  def connect(): Unit = {
    client.actor ! P2PClient.ConnectCommand
  }

  def isConnected()(implicit ec: ExecutionContext): Future[Boolean] = {
    client.isConnected()
  }

  def isInitialized()(implicit ec: ExecutionContext): Future[Boolean] = {
    client.isInitialized()
  }

  def isDisconnected()(implicit ec: ExecutionContext): Future[Boolean] = {
    client.isDisconnected()
  }

  /** Disconnects the given peer */
  def disconnect()(implicit ec: ExecutionContext): Future[Unit] = {
    isConnected().flatMap {
      case true =>
        logger.info(s"Disconnecting peer at socket=$socket")
        client.actor ! Tcp.Close
        Future.unit
      case false =>
        logger.warn(
          s"Cannot disconnect client that is not connected to socket=$socket!")
        Future.unit
    }
  }

  def sendInitMessage(): Future[Unit] = {
    val initTLV = InitTLV(ByteVector.empty, ByteVector.empty, Vector.empty)
    sendMsg(LnMessage(initTLV))
  }

  def sendDLCAcceptMessage(accept: DLCAccept): Future[Unit] = {
    sendMsg(accept.toMessage)
  }

  def sendDLCSignMessage(sign: DLCSign): Future[Unit] = {
    sendMsg(sign.toMessage)
  }

  private[dlc] def sendMsg(msg: LnMessage[TLV]): Future[Unit] = {
    logger.debug(s"Sending msg=${msg.typeName} to peer=$socket")
    client.actor ! msg
    Future.unit
  }
}
