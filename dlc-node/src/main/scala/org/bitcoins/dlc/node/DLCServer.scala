package org.bitcoins.dlc.node

import akka.actor.{Actor, ActorLogging, Props}
import akka.event.LoggingReceive
import akka.io.{IO, Tcp}
import org.bitcoins.dlc.node.DLCServer.GetState

import java.net.InetSocketAddress

class DLCServer(
    bindAddress: InetSocketAddress,
    dataHandlerFactory: DLCDataHandler.Factory = DLCDataHandler.defaultFactory)
    extends Actor
    with ActorLogging {

  private var state: DLCServer.State = DLCServer.Initializing

  import context.system

  IO(Tcp) ! Tcp.Bind(self, bindAddress)

  override def receive: Receive = LoggingReceive {
    case b @ Tcp.Bound(localAddress) =>
      state = DLCServer.Bound
      log.info(s"Bound at $localAddress")
      context.parent ! b

    case c @ Tcp.CommandFailed(_: Tcp.Bind) => {
      state = DLCServer.BindError(c.cause)
      val errorMessage = s"Cannot bind at ${bindAddress} "
      c.cause match {
        case Some(ex) =>
          log.error(errorMessage, ex)
        case None =>
          log.error(errorMessage)
      }
      context.stop(self)
    }

    case Tcp.Connected(remoteAddress, _) =>
      val connection = sender()
      log.info(s"Received a connection from $remoteAddress")
      val _ = context.actorOf(
        Props(new DLCConnectionHandler(connection, dataHandlerFactory)))

    case GetState => sender() ! state
  }
}

object DLCServer {

  sealed trait State
  case object Initializing extends State
  case object Bound extends State
  case class BindError(cause: Option[Throwable]) extends State

  object GetState

  def props(
      bindAddress: InetSocketAddress,
      dataHandlerFactory: DLCDataHandler.Factory) = Props(
    new DLCServer(bindAddress, dataHandlerFactory))
}
