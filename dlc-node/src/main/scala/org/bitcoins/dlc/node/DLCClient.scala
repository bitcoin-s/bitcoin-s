package org.bitcoins.dlc.node

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import akka.event.LoggingReceive
import akka.io.{IO, Tcp}
import org.bitcoins.dlc.node.peer.Peer
import org.bitcoins.tor.Socks5Connection.{Socks5Connect, Socks5Connected}
import org.bitcoins.tor.{Socks5Connection, Socks5ProxyParams}

import java.net.InetSocketAddress

class DLCClient(dataHandlerFactory: DLCDataHandler.Factory)
    extends Actor
    with ActorLogging {

  import context.system

  override def receive: Receive = LoggingReceive {
    case DLCClient.Connect(peer) =>
      val peerOrProxyAddress =
        peer.socks5ProxyParams match {
          case Some(proxyParams) =>
            val proxyAddress = proxyParams.address
            log.info(s"connecting to SOCKS5 proxy $proxyAddress")
            proxyAddress
          case None =>
            val remoteAddress = peer.socket
            log.info(s"connecting to $remoteAddress")
            remoteAddress
        }
      context.become(connecting(peer))
      IO(Tcp) ! Tcp.Connect(peerOrProxyAddress)
  }

  def connecting(peer: Peer): Receive = LoggingReceive {
    case c @ Tcp.CommandFailed(cmd: Tcp.Connect) =>
      val errorMessage = s"Cannot connect to ${cmd.remoteAddress} "
      c.cause match {
        case Some(ex) =>
          log.error(errorMessage, ex)

          c.cause.foreach(_.printStackTrace())
        case None =>
          log.error(errorMessage)
      }
      context.stop(self)

    case Tcp.Connected(peerOrProxyAddress, _) =>
      val connection = sender()
      peer.socks5ProxyParams match {
        case Some(proxyParams) =>
          val proxyAddress = peerOrProxyAddress
          val remoteAddress = peer.socket
          log.info(s"connected to SOCKS5 proxy $proxyAddress")
          log.info(s"connecting to $remoteAddress via SOCKS5 $proxyAddress")
          val proxy =
            context.actorOf(Socks5Connection.props(
                              sender(),
                              Socks5ProxyParams.proxyCredentials(proxyParams),
                              Socks5Connect(remoteAddress)),
                            "Socks5Connection")
          context watch proxy
          context become socks5Connecting(proxy,
                                          connection,
                                          remoteAddress,
                                          proxyAddress)
        case None =>
          val peerAddress = peerOrProxyAddress
          log.info(s"connected to $peerAddress")
          val _ = context.actorOf(
            Props(new DLCConnectionHandler(connection, dataHandlerFactory)))
      }
  }

  def socks5Connecting(
      proxy: ActorRef,
      connection: ActorRef,
      remoteAddress: InetSocketAddress,
      proxyAddress: InetSocketAddress): Receive = LoggingReceive {
    case Tcp.CommandFailed(_: Socks5Connect) =>
      log.error(s"connection failed to $remoteAddress via SOCKS5 $proxyAddress")
      context stop self
    case Socks5Connected(_) =>
      log.info(s"connected to $remoteAddress via SOCKS5 proxy $proxyAddress")
      context unwatch proxy
      val _ = context.actorOf(
        Props(new DLCConnectionHandler(connection, dataHandlerFactory)))
    case Terminated(actor) if actor == proxy =>
      context stop self
  }
}

object DLCClient {

  case class Connect(peer: Peer)

  def props(
      dataHandlerFactory: DLCDataHandler.Factory =
        DLCDataHandler.defaultFactory) = Props(
    new DLCClient(dataHandlerFactory))
}
