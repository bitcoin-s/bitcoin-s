package org.bitcoins.server

import akka.NotUsed
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import org.bitcoins.cli.{CliCommand, Config, ConsoleCli}
import org.bitcoins.commons.jsonmodels.ws.WalletNotification
import org.bitcoins.commons.jsonmodels.ws.WalletNotification.NewAddressNotification
import org.bitcoins.commons.serializers.WsPicklers
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.testkit.server.BitcoinSServerMainBitcoindFixture

import scala.concurrent.Future

class WebsocketTests extends BitcoinSServerMainBitcoindFixture {

  behavior of "Websocket Tests"

  def flow: Flow[Message, WalletNotification[_], NotUsed] = Flow[Message].map {
    case message: TextMessage.Strict =>
      //we should be able to parse the address message
      val text = message.text
      val notification: WalletNotification[_] =
        try {
          upickle.default.read[WalletNotification[_]](text)(
            WsPicklers.walletNotificationPickler)
        } catch {
          case err: Throwable =>
            logger.error(s"Failed to parse notification", err)
            throw err
        }
      notification
    case msg =>
      fail(s"Unexpected msg type received in the sink, msg=$msg")
  }

  it must "receive updates when an address is generated" in { server =>
    val req = WebSocketRequest("ws://localhost:19999/events")

    val sink: Sink[WalletNotification[_], Future[WalletNotification[_]]] =
      Sink.head[WalletNotification[_]]

    val cliConfig = Config(rpcPortOpt = Some(server.conf.rpcPort))

    //start the websocket
    val notificationF: Future[WalletNotification[_]] = {
      Http()
        .webSocketClientFlow(req)
        .viaMat(flow)(Keep.right)
        .runWith(Source.empty, sink)
        ._2
    }

    val expectedAddressStr = ConsoleCli
      .exec(CliCommand.GetNewAddress(labelOpt = None), cliConfig)
      .get
    val expectedAddress = BitcoinAddress.fromString(expectedAddressStr)
    for {
      notification <- notificationF
    } yield {
      notification match {
        case NewAddressNotification(actualAddress) =>
          assert(actualAddress == expectedAddress)
        case x =>
          fail(s"Expected address notitfication, got=$x")
      }
    }
  }
}
