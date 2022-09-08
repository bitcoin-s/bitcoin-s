package org.bitcoins.dlc.node

import akka.actor._
import akka.event.LoggingReceive
import akka.io.{IO, Tcp}
import grizzled.slf4j.Logging
import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.tor._

import java.io.IOException
import java.net.InetSocketAddress
import scala.concurrent.{Future, Promise}

class DLCServer(
    dlcWalletApi: DLCWalletApi,
    bindAddress: InetSocketAddress,
    boundAddress: Option[Promise[InetSocketAddress]],
    dataHandlerFactory: DLCDataHandler.Factory = DLCDataHandler.defaultFactory)
    extends Actor
    with ActorLogging {

  import context.system

  IO(Tcp) ! Tcp.Bind(self, bindAddress)

  var socket: ActorRef = _

  override def receive: Receive = LoggingReceive {
    case Tcp.Bound(localAddress) =>
      log.info(s"Bound at $localAddress")
      boundAddress.foreach(_.success(localAddress))
      socket = sender()

    case DLCServer.Disconnect =>
      socket ! Tcp.Unbind

    case c @ Tcp.CommandFailed(_: Tcp.Bind) =>
      val ex = c.cause.getOrElse(new IOException("Unknown Error"))
      log.error(s"Cannot bind $boundAddress", ex)
      throw ex

    case Tcp.Connected(remoteAddress, _) =>
      val connection = sender()
      log.info(s"Received a connection from $remoteAddress")
      val _ = context.actorOf(
        Props(
          new DLCConnectionHandler(dlcWalletApi,
                                   connection,
                                   None,
                                   dataHandlerFactory)))
  }

  override def postStop(): Unit = {
    super.postStop()
    socket ! Tcp.Unbind
  }

  override def aroundReceive(receive: Receive, msg: Any): Unit = try {
    super.aroundReceive(receive, msg)
  } catch {
    case t: Throwable =>
      boundAddress.foreach(_.tryFailure(t))
  }

}

object DLCServer extends Logging {

  case object Disconnect

  def props(
      dlcWalletApi: DLCWalletApi,
      bindAddress: InetSocketAddress,
      boundAddress: Option[Promise[InetSocketAddress]] = None,
      dataHandlerFactory: DLCDataHandler.Factory): Props = Props(
    new DLCServer(dlcWalletApi, bindAddress, boundAddress, dataHandlerFactory))

  def bind(
      dlcWalletApi: DLCWalletApi,
      bindAddress: InetSocketAddress,
      targets: Vector[InetSocketAddress],
      torParams: Option[TorParams],
      dataHandlerFactory: DLCDataHandler.Factory =
        DLCDataHandler.defaultFactory)(implicit
      system: ActorSystem): Future[(InetSocketAddress, ActorRef)] = {
    import system.dispatcher

    val promise = Promise[InetSocketAddress]()

    for {
      onionAddress <- torParams match {
        case Some(params) =>
          TorController
            .setUpHiddenService(
              params.controlAddress,
              params.authentication,
              params.privateKeyPath,
              bindAddress.getPort,
              targets.map(ip => s"${ip.getHostString}:${ip.getPort}")
            )
            .map(Some(_))
        case None =>
          logger.warn(
            s"Tor must be enabled to negotiate a dlc, you can set this with bitcoin-s.tor.enabled=true and bitcoin-s.control.enabled=true in your bitcoin-s.conf")
          Future.successful(None)
      }
      actorRef = system.actorOf(
        props(dlcWalletApi, bindAddress, Some(promise), dataHandlerFactory))
      boundAddress <- promise.future
    } yield {
      val addr = onionAddress.getOrElse(boundAddress)

      (addr, actorRef)
    }
  }
}
