package org.bitcoins.tor

import akka.actor.{
  Actor,
  ActorLogging,
  OneForOneStrategy,
  Props,
  SupervisorStrategy,
  Terminated
}
import akka.io.{IO, Tcp}
import akka.util.ByteString

import java.net.InetSocketAddress

/** Created by rorp
  *
  * @param address              Tor control address
  * @param protocolHandlerProps Tor protocol handler props
  * @param ec                   execution context
  */
class TorController(address: InetSocketAddress, protocolHandlerProps: Props)
    extends Actor
    with ActorLogging {

  import TorController._
  import Tcp._
  import context.system

  IO(Tcp) ! Connect(address)

  def receive = {
    case e @ CommandFailed(_: Connect) =>
      e.cause match {
        case Some(ex) => log.error(ex, "Cannot connect")
        case _        => log.error("Cannot connect")
      }
      context stop self
    case c: Connected =>
      val protocolHandler = context actorOf protocolHandlerProps
      protocolHandler ! c
      val connection = sender()
      connection ! Register(self)
      context watch connection
      context become {
        case data: ByteString =>
          connection ! Write(data)
        case CommandFailed(_: Write) =>
          // O/S buffer was full
          protocolHandler ! SendFailed
          log.error("Tor command failed")
        case Received(data) =>
          protocolHandler ! data
        case _: ConnectionClosed =>
          context stop self
        case Terminated(actor) if actor == connection =>
          context stop self
      }
  }

  // we should not restart a failing tor session
  override val supervisorStrategy = OneForOneStrategy(loggingEnabled = true) {
    case _ => SupervisorStrategy.Escalate
  }

}

object TorController {

  def props(address: InetSocketAddress, protocolHandlerProps: Props) =
    Props(new TorController(address, protocolHandlerProps))

  case object SendFailed

}
