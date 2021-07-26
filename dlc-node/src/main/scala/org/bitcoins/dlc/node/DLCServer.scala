package org.bitcoins.dlc.node

import akka.actor._
import akka.event.LoggingReceive
import akka.io.{IO, Tcp}
import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.dlc.node.config._
import org.bitcoins.tor.TorController

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
        Props(
          new DLCConnectionHandler(dlcWalletApi,
                                   connection,
                                   None,
                                   dataHandlerFactory)))
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
      dlcWalletApi: DLCWalletApi,
      bindAddress: InetSocketAddress,
      boundAddress: Option[Promise[InetSocketAddress]] = None,
      dataHandlerFactory: DLCDataHandler.Factory): Props = Props(
    new DLCServer(dlcWalletApi, bindAddress, boundAddress, dataHandlerFactory))

  def bind(
      dlcWalletApi: DLCWalletApi,
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
        system.actorOf(
          props(dlcWalletApi, bindAddress, Some(promise), dataHandlerFactory))
        promise.future
      }
    } yield {
      onionAddress.getOrElse(boundAddress)
    }
  }
}
