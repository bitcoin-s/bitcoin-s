package org.bitcoins.dlc.node

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.event.LoggingReceive
import akka.io.{IO, Tcp}
import org.bitcoins.tor.TorController
import org.bitcoins.tor.TorProtocolHandler.Authentication

import java.io.IOException
import java.net.InetSocketAddress
import java.nio.file.Path
import scala.concurrent.{Future, Promise}

class DLCServer(
    bindAddress: InetSocketAddress,
    boundAddress: Option[Promise[InetSocketAddress]],
    dataHandlerFactory: DLCDataHandler.Factory = DLCDataHandler.defaultFactory)
    extends Actor
    with ActorLogging {

  import context.system

  IO(Tcp) ! Tcp.Bind(self, bindAddress)

  override def receive: Receive = LoggingReceive {
    case b @ Tcp.Bound(localAddress) =>
      log.info(s"Bound at $localAddress")
      boundAddress.foreach(_.success(localAddress))
      context.parent ! b

    case c @ Tcp.CommandFailed(_: Tcp.Bind) =>
      val ex = c.cause.getOrElse(new IOException("Unknown Error"))
      log.error(s"Cannot bind $boundAddress", ex)
      throw ex

    case Tcp.Connected(remoteAddress, _) =>
      val connection = sender()
      log.info(s"Received a connection from $remoteAddress")
      val _ = context.actorOf(
        Props(new DLCConnectionHandler(connection, dataHandlerFactory)))
  }

  override def aroundReceive(receive: Receive, msg: Any): Unit = try {
    super.aroundReceive(receive, msg)
  } catch {
    case t: Throwable =>
      boundAddress.foreach(_.tryFailure(t))
  }

}

object DLCServer {

  def props(
      bindAddress: InetSocketAddress,
      boundAddress: Option[Promise[InetSocketAddress]] = None,
      dataHandlerFactory: DLCDataHandler.Factory): Props = Props(
    new DLCServer(bindAddress, boundAddress, dataHandlerFactory))

  case class TorParams(
      controlAddress: InetSocketAddress,
      authentication: Authentication,
      privateKeyPath: Path
  )

  def bind(
      bindAddress: InetSocketAddress,
      torParams: Option[TorParams],
      dataHandlerFactory: DLCDataHandler.Factory =
        DLCDataHandler.defaultFactory)(implicit
      system: ActorSystem): Future[InetSocketAddress] = {
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
              bindAddress.getPort
            )
            .map(Some(_))
        case None => Future.successful(None)
      }
      boundAddress <- {
        system.actorOf(props(bindAddress, Some(promise), dataHandlerFactory))
        promise.future
      }
    } yield {
      onionAddress.getOrElse(boundAddress)
    }
  }
}
