package org.bitcoins.dlc.node

import org.apache.pekko.actor.{
  Actor,
  ActorLogging,
  ActorRef,
  ActorSystem,
  Props
}
import org.apache.pekko.event.LoggingReceive
import org.apache.pekko.io.{IO, Tcp}
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.tor._
import scodec.bits.ByteVector

import java.io.IOException
import java.net.InetSocketAddress
import scala.concurrent.{Future, Promise}

class DLCServer(
    dlcWalletApi: DLCWalletApi,
    bindAddress: InetSocketAddress,
    boundAddress: Option[Promise[InetSocketAddress]],
    dataHandlerFactory: DLCDataHandler.Factory,
    handleWrite: (BigSizeUInt, ByteVector) => Future[Unit],
    handleWriteError: (BigSizeUInt, ByteVector, Throwable) => Future[Unit])
    extends Actor
    with ActorLogging {

  import context.system

  IO(Tcp) ! Tcp.Bind(self, bindAddress)

  private var socketOpt: Option[ActorRef] = None

  override def receive: Receive = LoggingReceive {
    case Tcp.Bound(localAddress) =>
      log.info(s"Bound at $localAddress")
      boundAddress.foreach(_.success(localAddress))
      socketOpt = Some(sender())

    case DLCServer.Disconnect =>
      socketOpt.map(_ ! Tcp.Unbind)
      socketOpt = None

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
                                   dataHandlerFactory,
                                   handleWrite,
                                   handleWriteError)))
  }

  override def postStop(): Unit = {
    super.postStop()
    socketOpt.map(_ ! Tcp.Unbind)
    socketOpt = None
  }

  override def aroundReceive(receive: Receive, msg: Any): Unit = try {
    super.aroundReceive(receive, msg)
  } catch {
    case t: Throwable =>
      boundAddress.foreach(_.tryFailure(t))
  }

}

object DLCServer extends BitcoinSLogger {

  case object Disconnect

  def props(
      dlcWalletApi: DLCWalletApi,
      bindAddress: InetSocketAddress,
      boundAddress: Option[Promise[InetSocketAddress]] = None,
      dataHandlerFactory: DLCDataHandler.Factory,
      handleWrite: (BigSizeUInt, ByteVector) => Future[Unit],
      handleWriteError: (BigSizeUInt, ByteVector, Throwable) => Future[Unit])
      : Props =
    Props(
      new DLCServer(dlcWalletApi,
                    bindAddress,
                    boundAddress,
                    dataHandlerFactory,
                    handleWrite,
                    handleWriteError))

  def bind(
      dlcWalletApi: DLCWalletApi,
      bindAddress: InetSocketAddress,
      targets: Vector[InetSocketAddress],
      torParams: Option[TorParams],
      handleWrite: (BigSizeUInt, ByteVector) => Future[Unit],
      handleWriteError: (BigSizeUInt, ByteVector, Throwable) => Future[Unit],
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
        props(dlcWalletApi,
              bindAddress,
              Some(promise),
              dataHandlerFactory,
              handleWrite,
              handleWriteError))
      boundAddress <- promise.future
    } yield {
      val addr = onionAddress.getOrElse(boundAddress)

      (addr, actorRef)
    }
  }
}
