package org.bitcoins.tor

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.ClientTransport.TCP
import akka.http.scaladsl.settings.ClientConnectionSettings
import akka.http.scaladsl.{ClientTransport, Http}
import akka.stream.scaladsl.{BidiFlow, Flow, Keep}
import akka.stream.stage._
import akka.stream.{Attributes, BidiShape, Inlet, Outlet}
import akka.util.ByteString

import java.net.InetSocketAddress
import scala.concurrent.Future
import scala.util.{Failure, Success}

class Socks5ClientTransport(proxyParams: Socks5ProxyParams)
    extends ClientTransport {

  override def connectTo(
      host: String,
      port: Int,
      settings: ClientConnectionSettings)(implicit system: ActorSystem): Flow[
    ByteString,
    ByteString,
    Future[Http.OutgoingConnection]] = {
    Socks5ProxyGraphStage(host, port, proxyParams)
      .joinMat(
        TCP.connectTo(proxyParams.address.getHostString,
                      proxyParams.address.getPort,
                      settings))(Keep.right)
      .mapMaterializedValue(_.map(_.copy(remoteAddress =
        InetSocketAddress.createUnresolved(host, port)))(system.dispatcher))
  }
}

object Socks5ProxyGraphStage {
  sealed trait State
  case object Greeting extends State
  case object Authenticating extends State
  case object Connecting extends State
  case object Connected extends State

  def apply(
      targetHostName: String,
      targetPort: Int,
      proxyParams: Socks5ProxyParams): BidiFlow[
    ByteString,
    ByteString,
    ByteString,
    ByteString,
    NotUsed] =
    BidiFlow.fromGraph(
      new Socks5ProxyGraphStage(targetHostName, targetPort, proxyParams))

}

class Socks5ProxyGraphStage(
    targetHostName: String,
    targetPort: Int,
    proxyParams: Socks5ProxyParams)
    extends GraphStage[
      BidiShape[ByteString, ByteString, ByteString, ByteString]] {

  val bytesIn: Inlet[ByteString] = Inlet("OutgoingTCP.in")
  val bytesOut: Outlet[ByteString] = Outlet("OutgoingTCP.out")

  val socks5In: Inlet[ByteString] = Inlet("OutgoingSOCKS5.in")
  val socks5Out: Outlet[ByteString] = Outlet("OutgoingSOCKS5.out")

  import Socks5Connection._
  import Socks5ProxyGraphStage._

  override def shape: BidiShape[
    ByteString,
    ByteString,
    ByteString,
    ByteString] = BidiShape.apply(socks5In, bytesOut, bytesIn, socks5Out)

  private val credentialsOpt = Socks5ProxyParams.proxyCredentials(proxyParams)

  private val greetingsMessage = socks5Greeting(credentialsOpt.isDefined)

  private val authMessage = credentialsOpt.map(c =>
    socks5PasswordAuthenticationRequest(c.username, c.password))

  private val connectMessage = socks5ConnectionRequest(
    InetSocketAddress.createUnresolved(targetHostName, targetPort))

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) with StageLogging {
      private var state: State = Greeting

      def send(message: ByteString): Unit = {
        emit(bytesOut, message, () => pull(bytesIn))
      }

      def sendGreeting(): Unit = {
        state = Greeting
        send(greetingsMessage)
      }

      def sendAuth(): Unit = {
        state = Authenticating
        authMessage match {
          case Some(message) => send(message)
          case None =>
            failStage(
              new IllegalStateException(
                "Cannot send AUTH message: undefined credentials"))
        }
      }

      def sendConnect(): Unit = {
        state = Connecting
        send(connectMessage)
      }

      def parseResponse(data: ByteString): Unit = {
        state match {
          case Greeting =>
            tryParseGreetings(data, credentialsOpt.nonEmpty) match {
              case Success(authMethod) =>
                if (authMethod == PasswordAuth) {
                  sendAuth()
                } else {
                  sendConnect()
                }
              case Failure(ex) =>
                failStage(ex)
            }
          case Authenticating =>
            tryParseAuth(data) match {
              case Success(authenticated) =>
                if (authenticated) {
                  sendConnect()
                } else {
                  failStage(new IllegalStateException("SOCKS5 AUTH failed"))
                }
              case Failure(ex) =>
                failStage(ex)
            }
          case Connecting =>
            tryParseConnectedAddress(data) match {
              case Success(_) =>
                state = Connected
                passAlong(bytesIn, socks5Out)
                passAlong(socks5In, bytesOut, doFinish = false, doPull = true)
                pull(bytesIn)
              case Failure(ex) =>
                failStage(ex)
            }
          case _ =>
            failStage(new IllegalStateException("Invalid state"))
        }
      }

      override def preStart(): Unit = {
        super.preStart()
        sendGreeting()
      }

      setHandler(bytesIn,
                 new InHandler {

                   override def onPush(): Unit = {
                     parseResponse(grab(bytesIn))
                   }
                 })

      setHandler(socks5In, eagerTerminateInput)
      setHandler(bytesOut, eagerTerminateOutput)
      setHandler(socks5Out, eagerTerminateOutput)
    }

}
