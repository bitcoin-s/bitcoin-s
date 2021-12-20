package org.bitcoins.server

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{
  Message,
  TextMessage,
  WebSocketRequest,
  WebSocketUpgradeResponse
}
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import org.bitcoins.cli.{CliCommand, Config, ConsoleCli}
import org.bitcoins.commons.jsonmodels.ws.WalletNotification
import org.bitcoins.commons.jsonmodels.ws.WalletNotification.{
  NewAddressNotification,
  TxBroadcastNotification
}
import org.bitcoins.commons.serializers.WsPicklers
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkit.server.{
  BitcoinSServerMainBitcoindFixture,
  ServerWithBitcoind
}

import scala.concurrent.{Future, Promise}

class WebsocketTests extends BitcoinSServerMainBitcoindFixture {

  behavior of "Websocket Tests"

  val endSink: Sink[WalletNotification[_], Future[Seq[WalletNotification[_]]]] =
    Sink.seq[WalletNotification[_]]

  val sink: Sink[Message, Future[Seq[WalletNotification[_]]]] = Flow[Message]
    .map {
      case message: TextMessage.Strict =>
        //we should be able to parse the address message
        val text = message.text
        val notification: WalletNotification[_] =
          upickle.default.read[WalletNotification[_]](text)(
            WsPicklers.walletNotificationPickler)
        notification
      case msg =>
        fail(s"Unexpected msg type received in the sink, msg=$msg")
    }
    .toMat(endSink)(Keep.right)

  val req = WebSocketRequest("ws://localhost:19999/events")

  it must "receive updates when an address is generated" in {
    serverWithBitcoind =>
      val ServerWithBitcoind(_, server) = serverWithBitcoind
      val cliConfig = Config(rpcPortOpt = Some(server.conf.rpcPort))
      //start the websocket
      val f: Flow[
        Message,
        Message,
        (Future[Seq[WalletNotification[_]]], Promise[Option[Message]])] = {
        Flow
          .fromSinkAndSourceCoupledMat(sink, Source.maybe[Message])(Keep.both)
      }

      val notificationsF: (
          Future[WebSocketUpgradeResponse],
          (Future[Seq[WalletNotification[_]]], Promise[Option[Message]])) = {
        Http()
          .singleWebSocketRequest(req, f)
      }

      val walletNotificationsF: Future[Seq[WalletNotification[_]]] =
        notificationsF._2._1

      val promise: Promise[Option[Message]] = notificationsF._2._2
      val expectedAddressStr = ConsoleCli
        .exec(CliCommand.GetNewAddress(labelOpt = None), cliConfig)
        .get
      val expectedAddress = BitcoinAddress.fromString(expectedAddressStr)

      promise.success(None)
      for {
        notifications <- walletNotificationsF
      } yield {
        assert(
          notifications.exists(_ == NewAddressNotification(expectedAddress)))
      }
  }

  it must "receive updates when a transaction is broadcast" in {
    serverWithBitcoind =>
      val ServerWithBitcoind(bitcoind, server) = serverWithBitcoind
      val cliConfig = Config(rpcPortOpt = Some(server.conf.rpcPort))
      val f: Flow[
        Message,
        Message,
        (Future[Seq[WalletNotification[_]]], Promise[Option[Message]])] = {
        Flow
          .fromSinkAndSourceCoupledMat(sink, Source.maybe[Message])(Keep.both)
      }

      val tuple: (
          Future[WebSocketUpgradeResponse],
          (Future[Seq[WalletNotification[_]]], Promise[Option[Message]])) = {
        Http()
          .singleWebSocketRequest(req, f)
      }

      val notificationsF = tuple._2._1
      val promise = tuple._2._2

      val addressF = bitcoind.getNewAddress

      for {
        address <- addressF
        cmd = CliCommand.SendToAddress(destination = address,
                                       amount = Bitcoins.one,
                                       satoshisPerVirtualByte =
                                         Some(SatoshisPerVirtualByte.one),
                                       noBroadcast = false)
        txIdStr = ConsoleCli.exec(cmd, cliConfig)
        expectedTxId = DoubleSha256DigestBE.fromHex(txIdStr.get)
        getTxCmd = CliCommand.GetTransaction(expectedTxId)
        expectedTxStr = ConsoleCli.exec(getTxCmd, cliConfig)
        expectedTx = Transaction.fromHex(expectedTxStr.get)
        _ = promise.success(None)
        notifications <- notificationsF
      } yield {
        assert(notifications.exists(_ == TxBroadcastNotification(expectedTx)))
      }
  }
}
