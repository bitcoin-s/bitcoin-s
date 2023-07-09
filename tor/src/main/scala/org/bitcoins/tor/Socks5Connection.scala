package org.bitcoins.tor

import akka.NotUsed
import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import akka.io.Tcp
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.util.ByteString
import grizzled.slf4j.Logging
import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.api.tor.Credentials
import org.bitcoins.tor.Socks5Connection.Socks5Connect

import java.net.{Inet4Address, Inet6Address, InetAddress, InetSocketAddress}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** Simple socks 5 client. It should be given a new connection, and will
  *
  * Created by rorp
  *
  * @param connection      underlying TcpConnection
  * @param credentialsOpt optional username/password for authentication
  */
class Socks5Connection(
    connection: ActorRef,
    credentialsOpt: Option[Credentials],
    target: Socks5Connect)
    extends Actor
    with ActorLogging
    with Logging {

  import Socks5Connection._

  context watch connection

  val passwordAuth: Boolean = credentialsOpt.isDefined

  var isConnected: Boolean = false

  connection ! Tcp.Register(self)
  connection ! Tcp.ResumeReading
  connection ! Tcp.Write(socks5Greeting(passwordAuth))

  override def receive: Receive = greetings

  def greetings: Receive = { case Tcp.Received(data) =>
    if (parseGreetings(data, passwordAuth) == PasswordAuth) {
      context become authenticate
      val credentials = credentialsOpt.getOrElse(
        throw Socks5Error("Credentials are not defined"))
      connection ! Tcp.Write(
        socks5PasswordAuthenticationRequest(credentials.username,
                                            credentials.password))
      connection ! Tcp.ResumeReading
    } else {
      context become connectionRequest
      connection ! Tcp.Write(socks5ConnectionRequest(target.address))
      connection ! Tcp.ResumeReading
    }
  }

  def authenticate: Receive = { case Tcp.Received(data) =>
    if (parseAuth(data)) {
      context become connectionRequest
      connection ! Tcp.Write(socks5ConnectionRequest(target.address))
      connection ! Tcp.ResumeReading
    }
  }

  def connectionRequest: Receive = { case Tcp.Received(data) =>
    val connectedAddressT = tryParseConnectedAddress(data)
    connectedAddressT match {
      case Success(connectedAddress) =>
        logger.info(
          s"Tor connection request succeeded. target=$target connectedAddress=$connectedAddress")
        context become connected
        context.parent ! Socks5Connected(connectedAddress)
        isConnected = true
      case Failure(err) =>
        logger.error(
          s"Tor connection request failed to $target errMsg=${err.toString}")
    }

  }

  def connected: Receive = { case Tcp.Register(handler, _, _) =>
    context become registered(handler)
  }

  def registered(handler: ActorRef): Receive = {
    case c: Tcp.Command => connection ! c
    case e: Tcp.Event   => handler ! e
  }

  override def unhandled(message: Any): Unit = message match {
    case Terminated(actor) if actor == connection => context stop self
    case _: Tcp.ConnectionClosed                  => context stop self
    case _                                        => log.warning(s"unhandled message=$message")
  }

  override def postStop(): Unit = {
    super.postStop()
    connection ! Tcp.Close
    if (!isConnected) {
      context.parent ! target.failureMessage
    }
  }

}

object Socks5Connection extends Logging {

  def props(
      tcpConnection: ActorRef,
      credentials_opt: Option[Credentials],
      command: Socks5Connect): Props = Props(
    new Socks5Connection(tcpConnection, credentials_opt, command))

  case class Socks5Connect(address: InetSocketAddress) extends Tcp.Command

  case class Socks5Connected(address: InetSocketAddress) extends Tcp.Event

  case class Socks5Error(message: String) extends RuntimeException(message)

  val NoAuth: Byte = 0x00
  val PasswordAuth: Byte = 0x02

  def socks5Greeting(passwordAuth: Boolean): ByteString = ByteString(
    0x05, // SOCKS version
    0x01, // number of authentication methods supported
    if (passwordAuth) PasswordAuth else NoAuth
  ) // auth method

  def socks5PasswordAuthenticationRequest(
      username: String,
      password: String): ByteString = {
    val usernameBytes = ByteString(username)
    val passwordBytes = ByteString(password)
    ByteString(0x01, // version of username/password authentication
               usernameBytes.length.toByte) ++
      usernameBytes ++
      ByteString(passwordBytes.length.toByte) ++
      passwordBytes
  }

  def socks5ConnectionRequest(address: InetSocketAddress): ByteString = {
    ByteString(0x05, // SOCKS version
               0x01, // establish a TCP/IP stream connection
               0x00) ++ // reserved
      addressToByteString(address) ++
      portToByteString(address.getPort)
  }

  def inetAddressToByteString(inet: InetAddress): ByteString = inet match {
    case a: Inet4Address =>
      ByteString(
        0x01 // IPv4 address
      ) ++ ByteString(a.getAddress)
    case a: Inet6Address =>
      ByteString(
        0x04 // IPv6 address
      ) ++ ByteString(a.getAddress)
    case _ => throw Socks5Error("Unknown InetAddress")
  }

  def addressToByteString(address: InetSocketAddress): ByteString = Option(
    address.getAddress) match {
    case None =>
      // unresolved address, use SOCKS5 resolver
      val host = address.getHostString
      ByteString(0x03, // Domain name
                 host.length.toByte) ++
        ByteString(host)
    case Some(inetAddress) =>
      inetAddressToByteString(inetAddress)
  }

  def portToByteString(port: Int): ByteString =
    ByteString((port & 0x0000ff00) >> 8, port & 0x000000ff)

  def parseGreetings(data: ByteString, passwordAuth: Boolean): Byte = {
    if (data(0) != 0x05) {
      throw Socks5Error("Invalid SOCKS5 version")
    } else if (
      (!passwordAuth && data(1) != NoAuth) || (passwordAuth && data(
        1) != PasswordAuth)
    ) {
      throw Socks5Error("Unsupported SOCKS5 auth method")
    } else {
      data(1)
    }
  }

  def parseAuth(data: ByteString): Boolean = {
    if (data(0) != 0x01) {
      throw Socks5Error("Invalid SOCKS5 auth method")
    } else if (data(1) != 0) {
      throw Socks5Error("SOCKS5 authentication failed")
    } else {
      true
    }
  }

  def tryParseConnectedAddress(data: ByteString): Try[InetSocketAddress] = {
    if (data(0) != 0x05) {
      throw Socks5Error("Invalid proxy version")
    } else {
      val status = data(1)
      if (status != 0) {
        val connectionError = TorConnectionError.fromByte(status)
        Failure(connectionError.exn)
      } else {
        data(3) match {
          case 0x01 =>
            val ip = Array(data(4), data(5), data(6), data(7))
            val port = data(8).toInt << 8 | data(9)
            val socket =
              new InetSocketAddress(InetAddress.getByAddress(ip), port)
            Success(socket)
          case 0x03 =>
            val len = data(4)
            val start = 5
            val end = start + len
            val domain = data.slice(start, end).utf8String
            val port = data(end).toInt << 8 | data(end + 1)
            val socket = new InetSocketAddress(domain, port)
            Success(socket)
          case 0x04 =>
            val ip = Array.ofDim[Byte](16)
            data.copyToArray(ip, 4, 4 + ip.length)
            val port = data(4 + ip.length).toInt << 8 | data(4 + ip.length + 1)
            val socket =
              new InetSocketAddress(InetAddress.getByAddress(ip), port)
            Success(socket)
          case b => Failure(Socks5Error(s"Unrecognized address type $b"))
        }
      }
    }
  }

  def tryParseGreetings(data: ByteString, passwordAuth: Boolean): Try[Byte] =
    Try(parseGreetings(data, passwordAuth))

  def tryParseAuth(data: ByteString): Try[Boolean] = Try(parseAuth(data))

  def socks5Handler(
      peer: Peer,
      sink: Sink[ByteString, NotUsed],
      onHandshakeComplete: () => Future[Unit],
      credentialsOpt: Option[Credentials])(implicit
      mat: Materializer,
      ec: ExecutionContext): Flow[ByteString, ByteString, NotUsed] = {
    Flow[ByteString]
      .statefulMap[Socks5ConnectionState,
                   Either[ByteString, Socks5MessageResponse]](() =>
        Socks5ConnectionState.Disconnected)(
        { case (state, bytes) =>
          state match {
            case Socks5ConnectionState.Disconnected =>
              if (
                parseGreetings(bytes, credentialsOpt.isDefined) == PasswordAuth
              ) {
                (Socks5ConnectionState.Authenticating,
                 Right(Socks5MessageResponse.Socks5GreetingResponse(bytes)))
              } else {
                (Socks5ConnectionState.Greeted,
                 Right(Socks5MessageResponse.Socks5GreetingResponse(bytes)))
              }
            case Socks5ConnectionState.Authenticating =>
              (Socks5ConnectionState.Greeted,
               Right(Socks5MessageResponse.Socks5AuthResponse(bytes)))
            case Socks5ConnectionState.Greeted =>
              (Socks5ConnectionState.Connected,
               Right(
                 Socks5MessageResponse.Socks5ConnectionRequestResponse(bytes)))
            case Socks5ConnectionState.Connected =>
              (Socks5ConnectionState.Connected, Left(bytes))
          }
        },
        _ => None // don't care about the end state, we don't emit it downstream
      )
      .mapAsync(1) {
        case Right(_: Socks5MessageResponse.Socks5GreetingResponse) =>
          credentialsOpt match {
            case Some(c) =>
              logger.debug(s"Authenticating socks5 proxy...")
              val authBytes =
                socks5PasswordAuthenticationRequest(c.username, c.password)
              Source.single(authBytes).runWith(sink)
              Future.successful(ByteString.empty)
            case None =>
              val connRequestBytes =
                Socks5Connection.socks5ConnectionRequest(peer.socket)
              logger.debug(s"Writing socks5 connection request")
              Source.single(connRequestBytes).runWith(sink)
              Future.successful(ByteString.empty)
          }
        case Right(authResponse: Socks5MessageResponse.Socks5AuthResponse) =>
          tryParseAuth(authResponse.byteString) match {
            case Success(true) =>
              val connRequestBytes =
                Socks5Connection.socks5ConnectionRequest(peer.socket)
              logger.debug(s"Writing socks5 connection request after auth")
              Source.single(connRequestBytes).runWith(sink)
              Future.successful(ByteString.empty)
            case Success(false) =>
              sys.error(s"Failed to authenticate with socks5 proxy")
            case Failure(err) => throw err
          }

        case Right(
              connReq: Socks5MessageResponse.Socks5ConnectionRequestResponse) =>
          val connectedAddressT =
            Socks5Connection.tryParseConnectedAddress(connReq.byteString)
          connectedAddressT match {
            case scala.util.Success(connectedAddress) =>
              logger.info(
                s"Tor connection request succeeded. target=${peer.socket} connectedAddress=$connectedAddress")
              onHandshakeComplete().map(_ => ByteString.empty)
            case scala.util.Failure(err) =>
              sys.error(
                s"Tor connection request failed to target=${peer.socket} errMsg=${err.toString}")
          }
        case Left(bytes) =>
          //after socks5 handshake done, pass bytes downstream to be parsed
          Future.successful(bytes)
      }
  }

}
sealed abstract class Socks5ConnectionState

object Socks5ConnectionState {
  case object Disconnected extends Socks5ConnectionState
  case object Greeted extends Socks5ConnectionState
  case object Authenticating extends Socks5ConnectionState

  case object Connected extends Socks5ConnectionState
}

sealed abstract class Socks5MessageResponse {
  def byteString: ByteString
}

object Socks5MessageResponse {

  case class Socks5GreetingResponse(byteString: ByteString)
      extends Socks5MessageResponse

  case class Socks5AuthResponse(byteString: ByteString)
      extends Socks5MessageResponse

  case class Socks5ConnectionRequestResponse(byteString: ByteString)
      extends Socks5MessageResponse
}
