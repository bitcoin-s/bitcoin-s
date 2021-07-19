package org.bitcoins.tor

import akka.actor.{
  Actor,
  ActorLogging,
  ActorSystem,
  OneForOneStrategy,
  Props,
  SupervisorStrategy,
  Terminated
}
import akka.io.{IO, Tcp}
import akka.util.ByteString
import org.bitcoins.tor.TorProtocolHandler.Authentication

import java.net.InetSocketAddress
import java.nio.file.Path
import scala.concurrent.{Future, Promise}

/** Created by rorp
  *
  * @param address              Tor control address
  * @param protocolHandlerProps Tor protocol handler props
  * @param ec                   execution context
  */
class TorController(address: InetSocketAddress, protocolHandlerProps: Props)
    extends Actor
    with ActorLogging {

  import Tcp._
  import TorController._
  import context.system

  IO(Tcp) ! Connect(address)

  override def receive: Receive = {
    case e @ CommandFailed(_: Connect) =>
      e.cause match {
        case Some(ex) => log.error(ex, "Cannot connect")
        case _        => log.error("Cannot connect")
      }
      context.stop(self)
    case c: Connected =>
      val protocolHandler = context.actorOf(protocolHandlerProps)
      protocolHandler ! c
      val connection = sender()
      connection ! Register(self)
      context.watch(connection)
      context.watch(protocolHandler)
      context.become {
        case data: ByteString =>
          connection ! Write(data)
        case c @ CommandFailed(_: Write) =>
          // O/S buffer was full
          protocolHandler ! SendFailed
          log.error("Tor command failed",
                    c.cause.getOrElse(new RuntimeException("Unknown error")))
        case Received(data) =>
          protocolHandler ! data
        case _: ConnectionClosed =>
          context.stop(self)
        case Terminated(actor) if actor == protocolHandler =>
          connection ! Tcp.Close
        case Terminated(actor) if actor == connection =>
          context.stop(self)
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

  /** Set up a Tor hidden service. It expects a Tor daemon is up and running,
    * and listening on the control address.
    *
    * Note: only Tor protocol v3 is supported
    *
    * @param controlAddress      Tor control address
    * @param authentication      Tor controller auth mechanism (password or safecookie)
    * @param privateKeyPath      path to a file that contains a Tor private key
    * @param virtualPort         port for the public hidden service (typically 9735)
    * @param targets             address of our protected server (format [host:]port), 127.0.0.1:[[virtualPort]] if empty
    */
  def setUpHiddenService(
      controlAddress: InetSocketAddress,
      authentication: Authentication,
      privateKeyPath: Path,
      virtualPort: Int,
      targets: Seq[String] = Seq())(implicit
      system: ActorSystem): Future[InetSocketAddress] = {

    val promiseTorAddress = Promise[InetSocketAddress]()

    val protocolHandlerProps = TorProtocolHandler.props(
      version = TorProtocolHandler.V3,
      authentication = authentication,
      privateKeyPath = privateKeyPath,
      virtualPort = virtualPort,
      targets = targets,
      onionAdded = Some(promiseTorAddress)
    )

    val _ = system.actorOf(
      TorController.props(address = controlAddress,
                          protocolHandlerProps = protocolHandlerProps),
      "tor"
    )

    promiseTorAddress.future
  }

}
