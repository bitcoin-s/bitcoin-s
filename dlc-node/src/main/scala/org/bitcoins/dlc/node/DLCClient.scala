package org.bitcoins.dlc.node

import akka.actor._
import akka.event.LoggingReceive
import akka.io.{IO, Tcp}
import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.dlc.node.peer.Peer
import org.bitcoins.tor.Socks5Connection.{Socks5Connect, Socks5Connected}
import org.bitcoins.tor.{Socks5Connection, Socks5ProxyParams}
import scodec.bits.ByteVector

import java.io.IOException
import java.net.InetSocketAddress
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}

class DLCClient(
    dlcWalletApi: DLCWalletApi,
    connectedAddress: Option[Promise[InetSocketAddress]],
    handlerP: Option[Promise[ActorRef]],
    dataHandlerFactory: DLCDataHandler.Factory,
    handleWrite: (BigSizeUInt, ByteVector) => Future[Unit],
    handleWriteError: (BigSizeUInt, ByteVector, Throwable) => Future[Unit])
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

      //currently our request timeout for requests sent to the backend is 60 seconds
      //so we need the connection timeout to occur before the request times out
      //when a user tries to submit an accept to the backend
      //see: https://github.com/bitcoin-s/bitcoin-s/issues/4080
      val connectionTimeout = 45.seconds
      IO(Tcp) ! Tcp.Connect(peerOrProxyAddress,
                            timeout = Some(connectionTimeout))
  }

  def connecting(peer: Peer): Receive = LoggingReceive {
    case c @ Tcp.CommandFailed(cmd: Tcp.Connect) =>
      val ex = c.cause.getOrElse(new IOException("Unknown Error"))
      log.error(s"Cannot connect to ${cmd.remoteAddress} ", ex)
      connectedAddress.foreach(_.failure(ex))
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
            Props(
              new DLCConnectionHandler(dlcWalletApi,
                                       connection,
                                       handlerP,
                                       dataHandlerFactory,
                                       handleWrite,
                                       handleWriteError)))
          connectedAddress.foreach(_.success(peerAddress))
      }
  }

  def socks5Connecting(
      proxy: ActorRef,
      remoteAddress: InetSocketAddress,
      proxyAddress: InetSocketAddress): Receive = LoggingReceive {
    case c @ Tcp.CommandFailed(_: Socks5Connect) =>
      val ex = c.cause.getOrElse(new IOException(
        s"Cannot connect to ${remoteAddress.getHostString}:${remoteAddress.getPort} via Tor"))
      log.error(s"connection failed to $remoteAddress via SOCKS5 $proxyAddress",
                ex)
      throw ex
    case Socks5Connected(_) =>
      log.info(s"connected to $remoteAddress via SOCKS5 proxy $proxyAddress")
      val _ = context.actorOf(
        Props(
          new DLCConnectionHandler(dlcWalletApi,
                                   proxy,
                                   handlerP,
                                   dataHandlerFactory,
                                   handleWrite,
                                   handleWriteError)))
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
      dlcWalletApi: DLCWalletApi,
      connectedAddress: Option[Promise[InetSocketAddress]],
      handlerP: Option[Promise[ActorRef]],
      dataHandlerFactory: DLCDataHandler.Factory,
      handleWrite: (BigSizeUInt, ByteVector) => Future[Unit],
      handleWriteError: (
          BigSizeUInt,
          ByteVector,
          Throwable) => Future[Unit]): Props =
    Props(
      new DLCClient(dlcWalletApi,
                    connectedAddress,
                    handlerP,
                    dataHandlerFactory,
                    handleWrite,
                    handleWriteError))

  def connect(
      peer: Peer,
      dlcWalletApi: DLCWalletApi,
      handlerP: Option[Promise[ActorRef]],
      dataHandlerFactory: DLCDataHandler.Factory =
        DLCDataHandler.defaultFactory,
      handleWrite: (BigSizeUInt, ByteVector) => Future[Unit],
      handleWriteError: (BigSizeUInt, ByteVector, Throwable) => Future[Unit])(
      implicit system: ActorSystem): Future[InetSocketAddress] = {
    val promise = Promise[InetSocketAddress]()
    val actor =
      system.actorOf(
        props(dlcWalletApi,
              Some(promise),
              handlerP,
              dataHandlerFactory,
              handleWrite,
              handleWriteError))
    actor ! Connect(peer)
    promise.future
  }
}
