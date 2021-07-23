package org.bitcoins.dlc.node

import akka.actor.{
  Actor,
  ActorLogging,
  ActorRef,
  ActorSystem,
  Props,
  Terminated
}
import akka.event.LoggingReceive
import akka.io.{IO, Tcp}
import org.bitcoins.dlc.node.peer.Peer
import org.bitcoins.tor.Socks5Connection.{Socks5Connect, Socks5Connected}
import org.bitcoins.tor.{Socks5Connection, Socks5ProxyParams}

import java.io.IOException
import java.net.InetSocketAddress
import scala.concurrent.{Future, Promise}

class DLCClient(
    connectedAddress: Option[Promise[InetSocketAddress]],
    dataHandlerFactory: DLCDataHandler.Factory)
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
      val ex = c.cause.getOrElse(new IOException("Unknown Error"))
      log.error(s"Cannot connect to ${cmd.remoteAddress} ", ex)
      throw ex

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
          context become socks5Connecting(proxy, remoteAddress, proxyAddress)
        case None =>
          val peerAddress = peerOrProxyAddress
          log.info(s"connected to $peerAddress")
          val _ = context.actorOf(
            Props(new DLCConnectionHandler(connection, dataHandlerFactory)))
          connectedAddress.foreach(_.success(peerAddress))
      }
  }

  def socks5Connecting(
      proxy: ActorRef,
      remoteAddress: InetSocketAddress,
      proxyAddress: InetSocketAddress): Receive = LoggingReceive {
    case c @ Tcp.CommandFailed(_: Socks5Connect) =>
      val ex = c.cause.getOrElse(new IOException("UnknownError"))
      log.error(s"connection failed to $remoteAddress via SOCKS5 $proxyAddress",
                ex)
      throw ex
    case Socks5Connected(_) =>
      log.info(s"connected to $remoteAddress via SOCKS5 proxy $proxyAddress")
      val _ = context.actorOf(
        Props(new DLCConnectionHandler(proxy, dataHandlerFactory)))
      connectedAddress.foreach(_.success(remoteAddress))
    case Terminated(actor) if actor == proxy =>
      context stop self
  }

  override def aroundReceive(receive: Receive, msg: Any): Unit = try {
    super.aroundReceive(receive, msg)
  } catch {
    case t: Throwable =>
      connectedAddress.foreach(_.tryFailure(t))
  }

}

object DLCClient {

  case class Connect(peer: Peer)

  def props(
      connectedAddress: Option[Promise[InetSocketAddress]],
      dataHandlerFactory: DLCDataHandler.Factory): Props = Props(
    new DLCClient(connectedAddress, dataHandlerFactory))

  def connect(
      peer: Peer,
      dataHandlerFactory: DLCDataHandler.Factory =
        DLCDataHandler.defaultFactory)(implicit
      system: ActorSystem): Future[InetSocketAddress] = {
    val promise = Promise[InetSocketAddress]()
    val actor = system.actorOf(props(Some(promise), dataHandlerFactory))
    actor ! Connect(peer)
    promise.future
  }
}
