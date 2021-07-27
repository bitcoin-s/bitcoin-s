package org.bitcoins.dlc.node

import akka.actor.ActorRef
import akka.testkit.{TestActorRef, TestProbe}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.number.UInt16
import org.bitcoins.core.protocol.tlv.{LnMessage, PingTLV, PongTLV}
import org.bitcoins.dlc.node.peer.Peer
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.util.BitcoinSActorFixtureWithDLCWallet
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import org.bitcoins.tor.{Socks5ProxyParams, TorController, TorProtocolHandler}
import org.scalatest.{Assertion, FutureOutcome}
import scodec.bits.ByteVector

import java.io.File
import java.net.{InetSocketAddress, Socket}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}
import scala.util.Try

class DLCServerTest extends BitcoinSActorFixtureWithDLCWallet {

  val torProxyAddress = new InetSocketAddress("localhost", 9050)
  val torControlAddress = new InetSocketAddress("localhost", 9051)
  val torProxyEnabled: Boolean = portIsBound(torProxyAddress)
  val torControlEnabled: Boolean = portIsBound(torControlAddress)

  override type FixtureParam = FundedDLCWallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFundedDLCWallet(test, getBIP39PasswordOpt())(getFreshConfig)
  }

  it must "send/receive Ping and Pong TLVs over clearnet" in { dlcWalletApi =>
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
        dlcWalletApi.wallet,
        bindAddress,
        Some(boundAddressPromise),
        { (_, _, connectionHandler) =>
          serverConnectionHandlerOpt = Some(connectionHandler)
          serverProbe.ref
        }
      ))

    val resultF: Future[Future[Assertion]] = for {
      _ <- boundAddressPromise.future
      connectedAddressPromise = Promise[InetSocketAddress]()
    } yield {
      var clientConnectionHandlerOpt = Option.empty[ActorRef]
      val clientProbe = TestProbe()
      val client = TestActorRef(
        DLCClient.props(dlcWalletApi.wallet,
                        Some(connectedAddressPromise),
                        None,
                        { (_, _, connectionHandler) =>
                          clientConnectionHandlerOpt = Some(connectionHandler)
                          clientProbe.ref
                        }))
      client ! DLCClient.Connect(Peer(connectAddress))

      for {
        _ <- connectedAddressPromise.future
        _ <- AsyncUtil.retryUntilSatisfied(serverConnectionHandlerOpt.isDefined)
        _ <- AsyncUtil.retryUntilSatisfied(clientConnectionHandlerOpt.isDefined)
        pingTLV =
          PingTLV(UInt16.one,
                  ByteVector.fromValidHex("00112233445566778899aabbccddeeff"))
        clientConnectionHandler = clientConnectionHandlerOpt.get
        _ = clientProbe.send(clientConnectionHandler, pingTLV)
        _ = serverProbe.expectMsg(LnMessage(pingTLV))
        pongTLV = PongTLV.forIgnored(
          ByteVector.fromValidHex("00112233445566778899aabbccddeeff"))
        serverConnectionHandler = serverConnectionHandlerOpt.get
        _ = serverProbe.send(serverConnectionHandler, pongTLV)
        _ = clientProbe.expectMsg(LnMessage(pongTLV))
        // 131063 - is a magic size for OS X when this test case starts failing (131073 overall TLV size)
        ignored = ByteVector.fill(65000)(0x55)
        bigTLV =
          PongTLV.forIgnored(ignored)
        _ = clientProbe.send(clientConnectionHandler, bigTLV)
        _ = serverProbe.expectMsg(LnMessage(bigTLV))
        _ = clientProbe.send(clientConnectionHandler,
                             DLCConnectionHandler.CloseConnection)
        _ = clientProbe.send(clientConnectionHandler, pingTLV)
        _ = serverProbe.expectNoMessage()
        _ = serverProbe.send(serverConnectionHandler, pongTLV)
        _ = clientProbe.expectNoMessage()
      } yield succeed
    }

    resultF.flatten
  }

  it must "send/receive Ping and Pong TLVs over Tor" in { fundedDLCWallet =>
    assume(torProxyEnabled, "Tor daemon is not running or listening port 9050")
    assume(torControlEnabled,
           "Tor daemon is not running or listening port 9051")

    val timeout = 30.seconds

    val resultF: Future[Assertion] = withTempFile("onion_", "_private_key") {
      pkFile =>
        val port = RpcUtil.randomPort
        val bindAddress =
          new InetSocketAddress("0.0.0.0", port)

        val connectAddressF = TorController.setUpHiddenService(
          torControlAddress,
          TorProtocolHandler.SafeCookie(),
          pkFile.toPath,
          port
        )

        var serverConnectionHandlerOpt = Option.empty[ActorRef]
        val serverProbe = TestProbe()

        val boundAddressPromise = Promise[InetSocketAddress]()

        TestActorRef(
          DLCServer.props(
            fundedDLCWallet.wallet,
            bindAddress,
            Some(boundAddressPromise),
            { (_, _, connectionHandler) =>
              serverConnectionHandlerOpt = Some(connectionHandler)
              serverProbe.ref
            }
          ))

        val resultF: Future[Future[Assertion]] = for {
          _ <- boundAddressPromise.future
          connectedAddressPromise = Promise[InetSocketAddress]()
          connectAddress <- connectAddressF
        } yield {
          var clientConnectionHandlerOpt = Option.empty[ActorRef]
          val clientProbe = TestProbe()
          val client = TestActorRef(
            DLCClient.props(
              fundedDLCWallet.wallet,
              Some(connectedAddressPromise),
              None,
              { (_, _, connectionHandler) =>
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

          for {
            _ <- connectedAddressPromise.future
            _ <- AsyncUtil.retryUntilSatisfied(
              serverConnectionHandlerOpt.isDefined)
            _ <- AsyncUtil.retryUntilSatisfied(
              clientConnectionHandlerOpt.isDefined)
            pingTLV =
              PingTLV(
                UInt16.one,
                ByteVector.fromValidHex("00112233445566778899aabbccddeeff"))
            clientConnectionHandler = clientConnectionHandlerOpt.get
            _ = clientProbe.send(clientConnectionHandler, pingTLV)
            _ = serverProbe.expectMsg(timeout, LnMessage(pingTLV))
            pongTLV = PongTLV.forIgnored(
              ByteVector.fromValidHex("00112233445566778899aabbccddeeff"))
            serverConnectionHandler = serverConnectionHandlerOpt.get
            _ = serverProbe.send(serverConnectionHandler, pongTLV)
            _ = clientProbe.expectMsg(timeout, LnMessage(pongTLV))
            // 131063 - is a magic size for OS X when this test case starts failing (131073 overall TLV size)
            ignored = ByteVector.fill(65000)(0x55)
            bigTLV = PongTLV.forIgnored(ignored)
            _ = clientProbe.send(clientConnectionHandler, bigTLV)
            _ = serverProbe.expectMsg(timeout, LnMessage(bigTLV))
            _ = clientProbe.send(clientConnectionHandler,
                                 DLCConnectionHandler.CloseConnection)
            _ = clientProbe.send(clientConnectionHandler, pingTLV)
            _ = serverProbe.expectNoMessage()
            _ = serverProbe.send(serverConnectionHandler, pongTLV)
            _ = clientProbe.expectNoMessage()
          } yield {
            succeed
          }
        }
        resultF.flatten
    }

    resultF
  }

  private def portIsBound(address: InetSocketAddress): Boolean =
    Try {
      val socket = new Socket(address.getHostString, address.getPort)
      socket.close()
    }.isSuccess

  private def withTempFile(
      prefix: String,
      suffix: String,
      create: Boolean = false)(
      f: File => Future[Assertion]): Future[Assertion] = {
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
