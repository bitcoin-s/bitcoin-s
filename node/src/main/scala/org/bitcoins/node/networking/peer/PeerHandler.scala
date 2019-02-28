package org.bitcoins.node.networking.peer

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.io.Tcp
import akka.pattern.ask
import akka.util.Timeout
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.DbConfig
import org.bitcoins.node.NetworkMessage
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.messages.{
  GetHeadersMessage,
  HeadersMessage,
  NetworkPayload
}
import org.bitcoins.node.networking.peer.PeerMessageSender.SendToPeer

import scala.concurrent.{ExecutionContext, Future}

/*
abstract class PeerHandler extends BitcoinSLogger {
  implicit val system: ActorSystem
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val timeout: Timeout

  def socket: InetSocketAddress

  def dbConfig: DbConfig

  def peerMsgSender: PeerMessageSender

  def getHeaders(getHeadersMsg: GetHeadersMessage): Unit = {
    sendToPeer(getHeadersMsg)
  }

  /** Connects with our peer*/
  def connect(): Future[Unit] = {
    pee
  }

  /** Checks if we are connected with our peer */
  def isConnected: Boolean = ???

  /** Closes our connection with our peer */
  def close(): Future[Unit] = {
    val closedF = (peerMsgSender.actor ? Tcp.Close).mapTo[Tcp.Closed.type]

    closedF.map(_ => ())
  }

}

object PeerHandler {
  private case class PeerHandlerImpl(
      peerMsgSender: PeerMessageSender,
      socket: InetSocketAddress,
      dbConfig: DbConfig)(
      override implicit val system: ActorSystem,
      val timeout: Timeout)
      extends PeerHandler

  def apply(
      peerMsgSender: PeerMessageSender,
      socket: InetSocketAddress,
      dbConfig: DbConfig)(
      implicit system: ActorSystem,
      timeout: Timeout): PeerHandler = {
    PeerHandlerImpl(peerMsgSender, socket, dbConfig)(system, timeout)
  }

  /*  def apply(peer: Peer, dbConfig: DbConfig)(implicit system: ActorSystem, timeout: Timeout): PeerHandler = {
    val actorRef = PeerMessageHandler(dbConfig = dbConfig)
    PeerHandler(actorRef,peer.socket,dbConfig)
  }*/
}
 */

case class PeerHandler(
    peerMsgRecv: PeerMessageReceiver,
    peerMsgSender: PeerMessageSender)
