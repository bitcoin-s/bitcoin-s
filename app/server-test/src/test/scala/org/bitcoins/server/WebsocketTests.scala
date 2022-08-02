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
import org.bitcoins.commons.rpc.{
  GetBlockHeader,
  GetNewAddress,
  GetTransaction,
  LockUnspent,
  Rescan,
  SendToAddress
}
import org.bitcoins.commons.serializers.{Picklers, WsPicklers}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.tlv.{DLCOfferTLV, LnMessage, LnMessageFactory}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.{CryptoUtil, DoubleSha256DigestBE}
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

      val str =
        "a71a006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000fdd82efd033900000000000186a0fda7204e" +
          "0012fda72642000400000000fda728020000fd88b8000000fda728020000fdafc8fdc3500000fda728020000fdd6d8fe000186a000" +
          "00fda728020000fe0003fffffe000186a00000fda724020000fda712fd02d9fdd824fd02d391177fd623a72d56e7bc12e3903f8d6b" +
          "ce7f07a25226d54009cd7e670f5e7a7320b0704286580d8b6a7f31ab7bf71356a13c28aa609a021111b2e3d2b2db26bc120bd29248" +
          "895b81f76b07d85a21b86021f22352d6376d19bbf5c93f918828f1fdd822fd026d0012fad0cde50a2258efa25cbba60ef0b6677cd2" +
          "9654802937c112097edb64bd205beea02263d6461e60a9ca8e08209c8bd5552863156014d5418cad91ac590bbf13a847f105db9899" +
          "d560e5040f9565c043c9e7fdf3782ad2708c5b99646c722b411854747248fb52e6486cce3eca5ddf9d64ecbe0864501a446efd3788" +
          "63f9a4055fab50d2112320ff14d8747a72467589822103f197063b49e77b90d82d3a8d49b63c3ceb9bd3328398a53989d4237216a2" +
          "4a1d12364efa2d2aec59cdc87909b115dca5b07106b70032ff78072f82ceeaf2e20db55086e9a2e5e5cac864992d747fd40f4b26bc" +
          "3d7de958ee02460d1199ff81438c9b76b3934cbc4566d10f242563b95e7df79c28d52c9c46b617676a4ee84a549ee1f0f53865c9ef" +
          "4d0ff825e2f438384c5f6238d0734beb570a1a49d035d9f86ee31c23f1e97bd34fba3f586c0fdf29997530e528b3200a0d7e34f865" +
          "dc4ca7bfd722bf77c0478ddd25bfa2dc6a4ab973d0c1b7a9ff38283b7c19bbe02677a9b628f3ee3d5198d66c1623c9608293093c12" +
          "6d4124a445bb6412f720493b6ffa411db53923895fd50c9563d50a97a86188084fe4c0f15ce50438ff0b00e1a9470185fd7c96296a" +
          "e2be12056f61ceaeee7ced48314a3b6855bc9aa3b446b9dfad68553f5302c60670a95fb9bdc5e72db7f1e9d42e4f4baca1bcbb2261" +
          "2db6417b45cc3d78b1ef33fc362a68db56df00ab1ee0700bd900200f6a24882101e71de7e18a7fb0d7da27b340de52f97d96239f35" +
          "9cfe31afcaf69cc9ddfcbfbdb2267e673ad728a29dd22d31d1a1187162037480fdd80a100002000642544355534400000000001212" +
          "446572696269742d4254432d394645423232036585c2349d728229d82c82b4d4e28d9889ccd430bbca1c949c042b93950f211f0016" +
          "00147e54f6d4148c0c0b4571c51cf624bf010e87418145a4e91696acf94e000000000000c3500002fda714fd0163876e1274389fa6" +
          "61014d02000000000101f798dcf8a5c9b0dca5d771ca5a0f9d882f0c7d2776925b3819e00f46daf699690000000000feffffff0220" +
          "02010000000000160014c799807edca63a977eeddd66d0fe07d147fefe4b808400000000000016001490222a528db0f1d8a1056286" +
          "b3b20c35c27c3b8704004730440220753db76fe9abafb01b141a36314abf530d7afd6365378c5bc036e0a735fe287402202f8f18cf" +
          "c1675d918d03a6de2855275aa3e4305e8f6e1b4cad1ae5dd31b9d5be014730440220221d4e91113ed01c3d4519c84efd11f51c543f" +
          "1efb4f658cd4e6ad69950dc44902206f3d9bfeae593c84975e27ba87d91ff0ed36bd15e0bc2d002d1370a51a61f89e01475221022b" +
          "8d44f97a4ecd80b33db307fc4874654c27e9812e0079d3f5c806a054ca756321039b921e070bc3ae42e73d8fb091ddf18c8f59b923" +
          "bdfa870347e83bc263ee4ea652ae003afa6100000000fffffffd006b0000fda714fd01635f8026cb666a3490014d02000000000101" +
          "9c66c927a1736790803e65167f9cfd618e4383cff635fd0af30d6a9af6897a3e0200000000feffffff02b7a7000000000000160014" +
          "c2d981b59e0374eeb1d9fca524e62e69170bd002e9de0000000000001600145f990a2987d844b3d5ff391c41b079f3935866b60400" +
          "4730440220735f325169ddd1a8e8828d3dd75386503055d5802156c07733a5d07d18a7219502204c7bab4e8b957fa95cc048205628" +
          "e9bd7f15f46df52306a39f596ae8df9d7e9c0147304402206cbbcea5def1ad4c937c2c6ac63346aef7e292974aa234890bd4c26eea" +
          "302dbe022019099e153c4000f46d75bd65ac769d3a1058b8d6c34953ec6804aa71e7f2132b0147522102b51a93f2196916782166e5" +
          "40260cb889b89e787256fd4d282ea25026abedb14a2103a8e77c9778e3ac62d2764668491f1febe0e92e5b270995d64e5aa39f64af" +
          "bd8252ae8074036200000000fffffffd006b0000001600145333b7c568cba36b5f53c24d05d36c076e741e9022edb0610ecaac5010" +
          "30b89bbde232b1000000000000000362037480620caf00"

      val offer: LnMessage[DLCOfferTLV] =
        LnMessageFactory(DLCOfferTLV).fromHex(str)

      val expectedHash = CryptoUtil.sha256(offer.tlv.bytes)

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
