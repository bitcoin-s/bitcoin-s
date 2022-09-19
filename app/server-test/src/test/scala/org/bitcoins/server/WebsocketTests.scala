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
import org.bitcoins.cli.ConsoleCli.exec
import org.bitcoins.cli.{CliCommand, Config, ConsoleCli}
import org.bitcoins.commons.jsonmodels.ws.ChainNotification.{
  BlockProcessedNotification,
  SyncFlagChangedNotification
}
import org.bitcoins.commons.jsonmodels.ws.WalletNotification._
import org.bitcoins.commons.jsonmodels.ws.{
  ChainNotification,
  WalletNotification,
  WalletWsType,
  WsNotification
}
import org.bitcoins.commons.rpc._
import org.bitcoins.commons.serializers.{Picklers, WsPicklers}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.tlv.{DLCOfferTLV, LnMessage, LnMessageFactory}
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
      val cliResponse = exec(GetNewAddress(labelOpt = None), cliConfig)

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
      val expectedAddressStr =
        exec(GetNewAddress(labelOpt = None), cliConfig).get
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
        cmd = SendToAddress(destination = address,
                            amount = Bitcoins.one,
                            satoshisPerVirtualByte =
                              Some(SatoshisPerVirtualByte.one),
                            noBroadcast = false)
        txIdStr = ConsoleCli.exec(cmd, cliConfig)
        expectedTxId = DoubleSha256DigestBE.fromHex(txIdStr.get)
        getTxCmd = GetTransaction(expectedTxId)
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
        cmd = SendToAddress(destination = address,
                            amount = Bitcoins.one,
                            satoshisPerVirtualByte =
                              Some(SatoshisPerVirtualByte.one),
                            noBroadcast = false)
        txIdStr = ConsoleCli.exec(cmd, cliConfig)
        expectedTxId = DoubleSha256DigestBE.fromHex(txIdStr.get)
        getTxCmd = GetTransaction(expectedTxId)
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
      cmd = GetBlockHeader(hash = hashes.head)
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
      val lockCmd = LockUnspent(unlock = false, Vector.empty)
      ConsoleCli.exec(lockCmd, cliConfig)

      //unlock all utxos
      val unlockCmd = LockUnspent(unlock = true, Vector.empty)
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

  it must "receive updates when an offer is added and removed" in {
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

      val str = "a71a000000010006226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f50a38b0f6bc6627a330f93ef6" +
        "2b1685e45d390f0c2e008784a494ae3f77e047500000000000bebc20000040161000000000bebc200016200000000000000000163000000" +
        "000bebc2000164000000000000000000fdd8249d288a4ac72f3f627ceecf61753f94c437f9e761950ce1dd4ad787cdf6f525ce11b6cea81" +
        "689ad41511d4366db5fb591b40864f59c4e9e0cf2c7dac89224d98c553d563caec479d618bad3cb0e844f57dcd977f23e5d6d84e1e3be51" +
        "bb33133cb0fdd8223900015c1785f8ab4273d56ac67d4b0429c40107cec5875246a2b68872792c2096e3a760bf0bb0fdd8060a000401610" +
        "1620163016404546573740284014ca41f49f56553b01d7da4f6c19afed76ac5d2fecde0bab6a878b57092ed001600148ac3370f8bb58401" +
        "12756ec4a48d4f417c958b6843e2078bcda6b45e0000000005f5e1000149fb24ec0ff14a9ca802000000000101000000000000000000000" +
        "0000000000000000000000000000000000000000000ffffffff03520101ffffffff0200f2052a01000000160014dbd4ce44e8f4db05f35c" +
        "a1c16378c56017b0582b0000000000000000266a24aa21a9ede2f61c3f71d1defd3fa999dfa36953755c690689799962b48bebd836974e8" +
        "cf9012000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffff006b000000160014" +
        "b742726c4817779988527052274d2a6f95c2cfb1da1acbdfc795a1b47d8adb8865f91fc9000000000000000260bf0bb060c84630"

      val offer: LnMessage[DLCOfferTLV] =
        LnMessageFactory(DLCOfferTLV).fromHex(str)

      val expectedHash = offer.tlv.tempContractId

      ConsoleCli
        .exec(
          CliCommand.AddDLCOffer(offer = offer, message = "msg", peer = "uri"),
          cliConfig)
        .get

      ConsoleCli
        .exec(CliCommand.RemoveDLCOffer(offerHash = expectedHash), cliConfig)
        .get

      for {
        _ <- AkkaUtil.nonBlockingSleep(500.millis)
        _ = promise.success(None)
        notifications <- walletNotificationsF
      } yield {
        val addNotification = notifications
          .find(_.isInstanceOf[DLCOfferAddNotification])
          .map(_.asInstanceOf[DLCOfferAddNotification])
        assert(addNotification.nonEmpty)
        assert(addNotification.get.payload.hash == expectedHash)

        val removeNotification = notifications
          .find(_.isInstanceOf[DLCOfferRemoveNotification])
          .map(_.asInstanceOf[DLCOfferRemoveNotification])
        assert(removeNotification.nonEmpty)
        assert(removeNotification.get.payload == expectedHash)
      }
  }

  it must "receive updates when a rescan is complete" in { serverWithBitcoind =>
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
    val notificationsF = tuple._2._1
    val promise = tuple._2._2
    val cmd = Rescan(batchSize = None,
                     startBlock = None,
                     endBlock = None,
                     force = true,
                     ignoreCreationTime = false)
    val _ = ConsoleCli.exec(cmd, cliConfig)
    for {
      _ <- AkkaUtil.nonBlockingSleep(5000.millis)
      _ = promise.success(None)
      notifications <- notificationsF
    } yield {
      val count = notifications.count(_.isInstanceOf[RescanComplete])
      assert(count == 1, s"count=$count")
    }
  }

  it must "receive updates when sync flag changes" in { serverWithBitcoind =>
    val ServerWithBitcoind(_, server) = serverWithBitcoind

    val req = buildReq(server.conf)
    val tuple: (
        Future[WebSocketUpgradeResponse],
        (Future[Seq[WsNotification[_]]], Promise[Option[Message]])) = {
      Http()
        .singleWebSocketRequest(req, websocketFlow)
    }
    val notificationsF = tuple._2._1
    val promise = tuple._2._2
    for {
      _ <- AkkaUtil.nonBlockingSleep(15.seconds)
      _ = promise.success(None)
      notifications <- notificationsF
    } yield {
      val syncingNotifications =
        notifications.filter(_.isInstanceOf[SyncFlagChangedNotification])
      assert(syncingNotifications.nonEmpty)
    }
  }

  it must "receive updates when fee rate changes" in { serverWithBitcoind =>
    val ServerWithBitcoind(_, server) = serverWithBitcoind

    val req = buildReq(server.conf)
    val tuple: (
        Future[WebSocketUpgradeResponse],
        (Future[Seq[WsNotification[_]]], Promise[Option[Message]])) = {
      Http()
        .singleWebSocketRequest(req, websocketFlow)
    }
    val notificationsF = tuple._2._1
    val promise = tuple._2._2
    for {
      _ <- AkkaUtil.nonBlockingSleep(2.seconds)
      _ = promise.success(None)
      notifications <- notificationsF
    } yield {
      val feeRateNotifications =
        notifications.filter(_.isInstanceOf[FeeRateChange])
      assert(feeRateNotifications.nonEmpty)
    }
  }

  /* TODO implement a real test for this case
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
   */
}
