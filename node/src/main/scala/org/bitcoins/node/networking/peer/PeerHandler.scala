package org.bitcoins.node.networking.peer

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.io.Tcp
import akka.pattern.ask
import akka.util.Timeout
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.NetworkMessage
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.db.DbConfig
import org.bitcoins.node.messages.{GetHeadersMessage, HeadersMessage, NetworkPayload}
import org.bitcoins.node.networking.peer.PeerMessageHandler.SendToPeer

import scala.concurrent.{ExecutionContext, Future}

abstract class PeerHandler extends BitcoinSLogger {
  implicit val system: ActorSystem
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val timeout: Timeout

  def socket: InetSocketAddress

  def dbConfig: DbConfig

  def peerActor: ActorRef

  def getHeaders(getHeadersMsg: GetHeadersMessage): Unit = {
    sendToPeer(getHeadersMsg)
  }

  /** Connects with our peer*/
  def connect(): Future[Unit] = {
    val handshakeF = (peerActor ? Tcp.Connect(socket)).mapTo[PeerMessageHandler.HandshakeFinished.type]
    handshakeF.map(_ => ())
  }

  /** Checks if we are connected with our peer */
  def isConnected: Boolean = ???

  /** Closes our connection with our peer */
  def close(): Future[Unit] = {
    val closedF = (peerActor ? Tcp.Close).mapTo[Tcp.Closed.type]

    closedF.map(_ => ())
  }

  /** Helper function to send a message to our peer on the p2p network */
  private def sendToPeer(payload: NetworkPayload) : Unit = {
    val networkMsg = NetworkMessage(Constants.networkParameters, payload)
    peerActor ! SendToPeer(networkMsg)
  }
}


object PeerHandler {
  private case class PeerHandlerImpl(peerActor: ActorRef, socket: InetSocketAddress,
                                     dbConfig: DbConfig)(override implicit val system: ActorSystem,
                                                         val timeout: Timeout) extends PeerHandler

  def apply(peerActor: ActorRef, socket: InetSocketAddress, dbConfig: DbConfig)(implicit system: ActorSystem, timeout: Timeout): PeerHandler = {
    PeerHandlerImpl(peerActor, socket, dbConfig)(system,timeout)
  }

  def apply(peer: Peer, dbConfig: DbConfig)(implicit system: ActorSystem, timeout: Timeout): PeerHandler = {
    val actorRef = PeerMessageHandler(dbConfig = dbConfig)
    PeerHandler(actorRef,peer.socket,dbConfig)
  }
}
