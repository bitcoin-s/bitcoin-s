package org.bitcoins.dlc.node

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import org.bitcoins.core.number.UInt16
import org.bitcoins.core.protocol.tlv.{LnMessage, PingTLV, PongTLV}
import org.bitcoins.dlc.node.peer.Peer
import org.bitcoins.rpc.util.RpcUtil
import org.scalatest._
import org.scalatest.funsuite.AnyFunSuiteLike
import scodec.bits.ByteVector

import java.net.InetSocketAddress
import scala.util.Try

class DLCServerTest
    extends TestKit(ActorSystem("test"))
    with TestSuite
    with BeforeAndAfterAll
    with AnyFunSuiteLike
    with ImplicitSender {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  test("send/receive Ping and Pong TLVs") {
    val port = RpcUtil.randomPort
    val bindAddress =
      new InetSocketAddress("0.0.0.0", port)
    val connectAddress =
      InetSocketAddress.createUnresolved("localhost", port)

    var serverConnectionHandlerOpt = Option.empty[ActorRef]
    val serverProbe = TestProbe()
    val server = TestActorRef(
      DLCServer.props(
        bindAddress,
        { (_, connectionHandler) =>
          serverConnectionHandlerOpt = Some(connectionHandler)
          serverProbe.ref
        }
      ))
    awaitCond {
      server ! DLCServer.GetState
      Try(expectMsg(DLCServer.Bound)).isSuccess
    }

    var clientConnectionHandlerOpt = Option.empty[ActorRef]
    val clientProbe = TestProbe()
    val client = TestActorRef(DLCClient.props { (_, connectionHandler) =>
      clientConnectionHandlerOpt = Some(connectionHandler)
      clientProbe.ref
    })
    client ! DLCClient.Connect(Peer(connectAddress))

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

}
