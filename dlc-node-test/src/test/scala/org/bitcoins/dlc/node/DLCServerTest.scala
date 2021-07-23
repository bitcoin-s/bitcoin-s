package org.bitcoins.dlc.node

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import org.bitcoins.core.number.UInt16
import org.bitcoins.core.protocol.tlv.{LnMessage, PingTLV, PongTLV}
import org.bitcoins.dlc.node.peer.Peer
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.tor.{Socks5ProxyParams, TorController, TorProtocolHandler}
import org.scalatest._
import org.scalatest.funsuite.AnyFunSuiteLike
import scodec.bits.ByteVector

import java.io.File
import java.net.{InetSocketAddress, Socket}
import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.DurationInt
import scala.util.Try

class DLCServerTest
    extends TestKit(ActorSystem("test"))
    with TestSuite
    with BeforeAndAfterAll
    with AnyFunSuiteLike
    with ImplicitSender {

  val torProxyAddress = new InetSocketAddress("localhost", 9050)
  val torControlAddress = new InetSocketAddress("localhost", 9051)
  val torProxyEnabled: Boolean = portIsBound(torProxyAddress)
  val torControlEnabled: Boolean = portIsBound(torControlAddress)

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  test("send/receive Ping and Pong TLVs over clearnet") {
    val port = RpcUtil.randomPort
    val bindAddress =
      new InetSocketAddress("0.0.0.0", port)
    val connectAddress =
      InetSocketAddress.createUnresolved("localhost", port)

    var serverConnectionHandlerOpt = Option.empty[ActorRef]
    val serverProbe = TestProbe()

    val boundAddressPromise = Promise[InetSocketAddress]()

    TestActorRef(
      DLCServer.props(
        bindAddress,
        Some(boundAddressPromise),
        { (_, connectionHandler) =>
          serverConnectionHandlerOpt = Some(connectionHandler)
          serverProbe.ref
        }
      ))

    Await.result(boundAddressPromise.future, 30.seconds)

    val connectedAddressPromise = Promise[InetSocketAddress]()

    var clientConnectionHandlerOpt = Option.empty[ActorRef]
    val clientProbe = TestProbe()
    val client = TestActorRef(
      DLCClient.props(Some(connectedAddressPromise),
                      { (_, connectionHandler) =>
                        clientConnectionHandlerOpt = Some(connectionHandler)
                        clientProbe.ref
                      }))
    client ! DLCClient.Connect(Peer(connectAddress))

    Await.result(connectedAddressPromise.future, 30.seconds)

    awaitCond(serverConnectionHandlerOpt.isDefined)
    awaitCond(clientConnectionHandlerOpt.isDefined)

    val pingTLV =
      PingTLV(UInt16.one,
              ByteVector.fromValidHex("00112233445566778899aabbccddeeff"))

    val clientConnectionHandler = clientConnectionHandlerOpt.get
    clientProbe.send(clientConnectionHandler, pingTLV)
    serverProbe.expectMsg(LnMessage(pingTLV))

    val pongTLV = PongTLV.forIgnored(
      ByteVector.fromValidHex("00112233445566778899aabbccddeeff"))

    val serverConnectionHandler = serverConnectionHandlerOpt.get
    serverProbe.send(serverConnectionHandler, pongTLV)
    clientProbe.expectMsg(LnMessage(pongTLV))

    // 131063 - is a magic size for OS X when this test case starts failing (131073 overall TLV size)
    val ignored = ByteVector.fill(65000)(0x55)
    val bigTLV =
      PongTLV.forIgnored(ignored)

    clientProbe.send(clientConnectionHandler, bigTLV)
    serverProbe.expectMsg(LnMessage(bigTLV))

    clientProbe.send(clientConnectionHandler,
                     DLCConnectionHandler.CloseConnection)
    clientProbe.send(clientConnectionHandler, pingTLV)
    serverProbe.expectNoMessage()
    serverProbe.send(serverConnectionHandler, pongTLV)
    clientProbe.expectNoMessage()

  }

  test("send/receive Ping and Pong TLVs over Tor") {
    assume(torProxyEnabled, "Tor daemon is not running or listening port 9050")
    assume(torControlEnabled,
           "Tor daemon is not running or listening port 9051")

    val timeout = 30.seconds

    withTempFile("onion_", "_private_key") { pkFile =>
      val port = RpcUtil.randomPort
      val bindAddress =
        new InetSocketAddress("0.0.0.0", port)

      val connectAddress = Await.result(TorController.setUpHiddenService(
                                          torControlAddress,
                                          TorProtocolHandler.SafeCookie(),
                                          pkFile.toPath,
                                          port
                                        ),
                                        timeout)

      var serverConnectionHandlerOpt = Option.empty[ActorRef]
      val serverProbe = TestProbe()

      val boundAddressPromise = Promise[InetSocketAddress]()

      TestActorRef(
        DLCServer.props(
          bindAddress,
          Some(boundAddressPromise),
          { (_, connectionHandler) =>
            serverConnectionHandlerOpt = Some(connectionHandler)
            serverProbe.ref
          }
        ))

      Await.result(boundAddressPromise.future, timeout)

      val connectedAddressPromise = Promise[InetSocketAddress]()

      var clientConnectionHandlerOpt = Option.empty[ActorRef]
      val clientProbe = TestProbe()
      val client = TestActorRef(
        DLCClient.props(
          Some(connectedAddressPromise),
          { (_, connectionHandler) =>
            clientConnectionHandlerOpt = Some(connectionHandler)
            clientProbe.ref
          }
        ))
      client ! DLCClient.Connect(
        Peer(connectAddress,
             socks5ProxyParams = Some(
               Socks5ProxyParams(
                 address = torProxyAddress,
                 credentialsOpt = None,
                 randomizeCredentials = true
               ))))

      Await.result(connectedAddressPromise.future, timeout)

      awaitCond(serverConnectionHandlerOpt.isDefined, max = timeout)
      awaitCond(clientConnectionHandlerOpt.isDefined, max = timeout)

      val pingTLV =
        PingTLV(UInt16.one,
                ByteVector.fromValidHex("00112233445566778899aabbccddeeff"))

      val clientConnectionHandler = clientConnectionHandlerOpt.get
      clientProbe.send(clientConnectionHandler, pingTLV)
      serverProbe.expectMsg(timeout, LnMessage(pingTLV))

      val pongTLV = PongTLV.forIgnored(
        ByteVector.fromValidHex("00112233445566778899aabbccddeeff"))

      val serverConnectionHandler = serverConnectionHandlerOpt.get
      serverProbe.send(serverConnectionHandler, pongTLV)
      clientProbe.expectMsg(timeout, LnMessage(pongTLV))

      // 131063 - is a magic size for OS X when this test case starts failing (131073 overall TLV size)
      val ignored = ByteVector.fill(65000)(0x55)
      val bigTLV =
        PongTLV.forIgnored(ignored)

      clientProbe.send(clientConnectionHandler, bigTLV)
      serverProbe.expectMsg(timeout, LnMessage(bigTLV))

      clientProbe.send(clientConnectionHandler,
                       DLCConnectionHandler.CloseConnection)
      clientProbe.send(clientConnectionHandler, pingTLV)
      serverProbe.expectNoMessage()
      serverProbe.send(serverConnectionHandler, pongTLV)
      clientProbe.expectNoMessage()
    }
  }

  private def portIsBound(address: InetSocketAddress): Boolean =
    Try {
      val socket = new Socket(address.getHostString, address.getPort)
      socket.close()
    }.isSuccess

  private def withTempFile[A](
      prefix: String,
      suffix: String,
      create: Boolean = false)(f: (File) => A): A = {
    val tempFile = File.createTempFile(prefix, suffix)
    if (!create) {
      tempFile.delete()
    }
    try {
      f(tempFile)
    } finally {
      val _ = tempFile.delete()
    }
  }

}
