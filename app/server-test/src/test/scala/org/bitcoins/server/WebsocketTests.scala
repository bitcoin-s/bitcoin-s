package org.bitcoins.server

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials}
import akka.http.scaladsl.model.ws.{
  Message,
  TextMessage,
  WebSocketRequest,
  WebSocketUpgradeResponse
}
import akka.http.scaladsl.model.{HttpHeader, StatusCodes}
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import org.bitcoins.cli.{CliCommand, Config, ConsoleCli}
import org.bitcoins.commons.jsonmodels.ws.ChainNotification.BlockProcessedNotification
import org.bitcoins.commons.jsonmodels.ws.WalletNotification.{
  NewAddressNotification,
  TxBroadcastNotification,
  TxProcessedNotification
}
import org.bitcoins.commons.jsonmodels.ws.{
  ChainNotification,
  WalletNotification,
  WalletWsType,
  WsNotification
}
import org.bitcoins.commons.serializers.{Picklers, WsPicklers}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkit.server.{
  BitcoinSServerMainBitcoindFixture,
  ServerWithBitcoind
}
import org.bitcoins.testkit.util.AkkaUtil

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}
import scala.util.Try

class WebsocketTests extends BitcoinSServerMainBitcoindFixture {

  behavior of "Websocket Tests"

  val endSink: Sink[WsNotification[_], Future[Seq[WsNotification[_]]]] =
    Sink.seq[WsNotification[_]]

  val sink: Sink[Message, Future[Seq[WsNotification[_]]]] = Flow[Message]
    .map {
      case message: TextMessage.Strict =>
        //we should be able to parse the address message
        val text = message.text
        val walletNotificationOpt: Option[WalletNotification[_]] = Try(
          upickle.default.read[WalletNotification[_]](text)(
            WsPicklers.walletNotificationPickler)).toOption
        val chainNotificationOpt: Option[ChainNotification[_]] = Try(
          upickle.default.read[ChainNotification[_]](text)(
            WsPicklers.chainNotificationPickler)).toOption
        walletNotificationOpt.getOrElse(chainNotificationOpt.get)
      case msg =>
        fail(s"Unexpected msg type received in the sink, msg=$msg")
    }
    .toMat(endSink)(Keep.right)

  def buildReq(
      conf: BitcoinSAppConfig,
      rpcPassword: Option[String] = None): WebSocketRequest = {
    val headers: Vector[HttpHeader] = Vector(
      Authorization(
        BasicHttpCredentials("bitcoins",
                             rpcPassword.getOrElse(conf.rpcPassword))))
    WebSocketRequest(s"ws://localhost:${conf.wsPort}/events",
                     extraHeaders = headers)
  }

  val websocketFlow: Flow[
    Message,
    Message,
    (Future[Seq[WsNotification[_]]], Promise[Option[Message]])] = {
    Flow
      .fromSinkAndSourceCoupledMat(sink, Source.maybe[Message])(Keep.both)
  }

  it must "fail if RPC password is incorrect" in { serverWithBitcoind =>
    val ServerWithBitcoind(_, server) = serverWithBitcoind
    val req = buildReq(server.conf, Some("wrong password"))
    val notificationsF = Http().singleWebSocketRequest(req, websocketFlow)

    for {
      response <- notificationsF._1
    } yield {
      assert(response.response.status == StatusCodes.Unauthorized)

      val cliConfig = Config(rpcPortOpt = Some(server.conf.rpcPort),
                             rpcPassword = "wrong password")
      val cliResponse =
        ConsoleCli.exec(CliCommand.GetNewAddress(labelOpt = None), cliConfig)

      assert(cliResponse.isFailure)
      assert(
        cliResponse.failed.get.getMessage == "The supplied authentication is invalid")
    }
  }

  it must "receive updates when an address is generated" in {
    serverWithBitcoind =>
      val ServerWithBitcoind(_, server) = serverWithBitcoind
      val cliConfig = Config(rpcPortOpt = Some(server.conf.rpcPort),
                             rpcPassword = server.conf.rpcPassword)

      val req = buildReq(server.conf)
      val notificationsF: (
          Future[WebSocketUpgradeResponse],
          (Future[Seq[WsNotification[_]]], Promise[Option[Message]])) = {
        Http()
          .singleWebSocketRequest(req, websocketFlow)
      }

      val walletNotificationsF: Future[Seq[WsNotification[_]]] =
        notificationsF._2._1

      val promise: Promise[Option[Message]] = notificationsF._2._2
      val expectedAddressStr = ConsoleCli
        .exec(CliCommand.GetNewAddress(labelOpt = None), cliConfig)
        .get
      val expectedAddress = BitcoinAddress.fromString(expectedAddressStr)

      for {
        _ <- AkkaUtil.nonBlockingSleep(500.millis)
        _ = promise.success(None)
        notifications <- walletNotificationsF
      } yield {
        assert(
          notifications.exists(_ == NewAddressNotification(expectedAddress)))
      }
  }

  it must "receive updates when a transaction is broadcast" in {
    serverWithBitcoind =>
      val ServerWithBitcoind(bitcoind, server) = serverWithBitcoind
      val cliConfig = Config(rpcPortOpt = Some(server.conf.rpcPort),
                             rpcPassword = server.conf.rpcPassword)

      val req = buildReq(server.conf)
      val tuple: (
          Future[WebSocketUpgradeResponse],
          (Future[Seq[WsNotification[_]]], Promise[Option[Message]])) = {
        Http()
          .singleWebSocketRequest(req, websocketFlow)
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
        _ <- AkkaUtil.nonBlockingSleep(500.millis)
        _ = promise.success(None)
        notifications <- notificationsF
      } yield {
        assert(notifications.exists(_ == TxBroadcastNotification(expectedTx)))
      }
  }

  it must "receive updates when a transaction is processed" in {
    serverWithBitcoind =>
      val ServerWithBitcoind(bitcoind, server) = serverWithBitcoind
      val cliConfig = Config(rpcPortOpt = Some(server.conf.rpcPort),
                             rpcPassword = server.conf.rpcPassword)

      val req = buildReq(server.conf)
      val tuple: (
          Future[WebSocketUpgradeResponse],
          (Future[Seq[WsNotification[_]]], Promise[Option[Message]])) = {
        Http()
          .singleWebSocketRequest(req, websocketFlow)
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
        _ <- AkkaUtil.nonBlockingSleep(500.millis)
        _ = promise.success(None)
        notifications <- notificationsF
      } yield {
        assert(notifications.exists(_ == TxProcessedNotification(expectedTx)))
      }
  }

  it must "receive updates when a block is processed" in { serverWithBitcoind =>
    val ServerWithBitcoind(bitcoind, server) = serverWithBitcoind
    val cliConfig = Config(rpcPortOpt = Some(server.conf.rpcPort),
                           rpcPassword = server.conf.rpcPassword)

    val req = buildReq(server.conf)
    val tuple: (
        Future[WebSocketUpgradeResponse],
        (Future[Seq[WsNotification[_]]], Promise[Option[Message]])) = {
      Http()
        .singleWebSocketRequest(req, websocketFlow)
    }

    val notificationsF = tuple._2._1
    val promise = tuple._2._2

    val addressF = bitcoind.getNewAddress
    val timeout =
      15.seconds //any way we can remove this timeout and just check?
    for {
      address <- addressF
      hashes <- bitcoind.generateToAddress(1, address)
      cmd = CliCommand.GetBlockHeader(hash = hashes.head)
      getBlockHeaderResultStr = ConsoleCli.exec(cmd, cliConfig)
      getBlockHeaderResult = upickle.default.read(getBlockHeaderResultStr.get)(
        Picklers.getBlockHeaderResultPickler)
      _ <- AkkaUtil.nonBlockingSleep(timeout)
      _ = promise.success(None)
      notifications <- notificationsF
    } yield {
      val count = notifications.count(
        _ == BlockProcessedNotification(getBlockHeaderResult))
      assert(count == 1, s"count=$count")
    }
  }

  it must "get notifications for reserving and unreserving utxos" in {
    serverWithBitcoind =>
      val ServerWithBitcoind(_, server) = serverWithBitcoind
      val cliConfig = Config(rpcPortOpt = Some(server.conf.rpcPort),
                             rpcPassword = server.conf.rpcPassword)

      val req = buildReq(server.conf)
      val tuple: (
          Future[WebSocketUpgradeResponse],
          (Future[Seq[WsNotification[_]]], Promise[Option[Message]])) = {
        Http()
          .singleWebSocketRequest(req, websocketFlow)
      }

      val notificationsF: Future[Seq[WsNotification[_]]] = tuple._2._1
      val promise = tuple._2._2

      //lock all utxos
      val lockCmd = CliCommand.LockUnspent(unlock = false, Vector.empty)
      ConsoleCli.exec(lockCmd, cliConfig)

      //unlock all utxos
      val unlockCmd = CliCommand.LockUnspent(unlock = true, Vector.empty)
      ConsoleCli.exec(unlockCmd, cliConfig)

      for {
        _ <- AkkaUtil.nonBlockingSleep(500.millis)
        _ = promise.success(None)
        notifications <- notificationsF
      } yield {
        //should have two notifications for locking and then unlocking the utxos
        assert(notifications.count(_.`type` == WalletWsType.ReservedUtxos) == 2)
      }
  }

  it must "not queue things on the websocket while there is no one connected" in {
    serverWithBitcoind =>
      val ServerWithBitcoind(_, server) = serverWithBitcoind

      val req = buildReq(server.conf)
      val tuple: (
          Future[WebSocketUpgradeResponse],
          (Future[Seq[WsNotification[_]]], Promise[Option[Message]])) = {
        Http()
          .singleWebSocketRequest(req, websocketFlow)
      }

      val notificationsF: Future[Seq[WsNotification[_]]] = tuple._2._1
      val promise = tuple._2._2

      for {
        _ <- AkkaUtil.nonBlockingSleep(2.seconds)
        _ = promise.success(None)
        notifications <- notificationsF
      } yield assert(notifications.isEmpty)
  }
}
