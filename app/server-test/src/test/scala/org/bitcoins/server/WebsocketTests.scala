package org.bitcoins.server

import akka.{Done, NotUsed}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.stream.scaladsl.{Sink, Source}
import org.bitcoins.cli.{CliCommand, Config, ConsoleCli}
import org.bitcoins.commons.jsonmodels.ws.{WalletNotification, WalletWsType}
import org.bitcoins.commons.serializers.WsPicklers
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.testkit.server.BitcoinSServerMainBitcoindFixture

import scala.concurrent.{Future, Promise}

class WebsocketTests extends BitcoinSServerMainBitcoindFixture {

  behavior of "Websocket Tests"

  it must "receive updates when an address is generated" in { server =>
    val req = WebSocketRequest("ws://localhost:19999/events")
    // print each incoming strict text message
    val p = Promise[BitcoinAddress]()

    val sink: Sink[Message, Future[Done]] =
      Sink.foreach {
        case message: TextMessage.Strict =>
          //we should be able to parse the address message
          val text = message.text
          println(s"text=$text")
          val notification: WalletNotification =
            try {
              upickle.default.read[WalletNotification](text)(
                WsPicklers.walletNotificationPickler)
            } catch {
              case err: Throwable =>
                logger.error(s"Failed to parse notification", err)
                throw err
            }
          notification.`type` match {
            case WalletWsType.NewAddress =>
              val addressString = notification.payload.str
              val address = BitcoinAddress.fromString(addressString)
              p.success(address)
            case x @ (WalletWsType.ReservedUtxos | WalletWsType.TxProcessed |
                WalletWsType.TxBroadcast | WalletWsType.ReservedUtxos) =>
              fail(
                s"Received unexpected websocket type in address test, got=$x")
          }
        case _ =>
      }

    val cliConfig = Config(rpcPortOpt = Some(server.conf.rpcPort))

    //start the websocket
    val _: NotUsed = Http()
      .webSocketClientFlow(req)
      .to(sink)
      .runWith(Source.empty)

    val address = ConsoleCli
      .exec(CliCommand.GetNewAddress(labelOpt = None), cliConfig)
      .get

    for {
      wsBitcoinAddress <- p.future
    } yield {
      assert(BitcoinAddress.fromString(address) == wsBitcoinAddress)
    }
  }
}
