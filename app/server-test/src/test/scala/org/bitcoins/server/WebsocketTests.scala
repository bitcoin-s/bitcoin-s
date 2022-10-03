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
import org.bitcoins.commons.jsonmodels.ws.DLCNodeNotification.{
  DLCAcceptFailed,
  DLCNodeConnectionFailed,
  DLCNodeConnectionInitiated
}
import org.bitcoins.commons.jsonmodels.ws.WalletNotification._
import org.bitcoins.commons.jsonmodels.ws._
import org.bitcoins.commons.rpc._
import org.bitcoins.commons.serializers.{Picklers, WsPicklers}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.tlv.{DLCOfferTLV, LnMessage}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.{CryptoUtil, DoubleSha256DigestBE}
import org.bitcoins.testkit.server.{
  BitcoinSServerMainBitcoindFixture,
  ServerWithBitcoind
}
import org.bitcoins.testkit.util.AkkaUtil

import java.net.InetSocketAddress
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
        val dlcNodeNotificationOpt: Option[DLCNodeNotification[_]] = Try(
          upickle.default.read[DLCNodeNotification[_]](text)(
            WsPicklers.dlcNodeNotificationPickler)).toOption
        val walletNotificationOpt: Option[WalletNotification[_]] = Try(
          upickle.default.read[WalletNotification[_]](text)(
            WsPicklers.walletNotificationPickler)).toOption
        val chainNotificationOpt: Option[ChainNotification[_]] = Try(
          upickle.default.read[ChainNotification[_]](text)(
            WsPicklers.chainNotificationPickler)).toOption
        walletNotificationOpt.getOrElse(
          chainNotificationOpt.getOrElse(dlcNodeNotificationOpt.get))
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

  val str =
    "fda71afd055b0006226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910ffdd82efd041f000000000bebc200" +
      "fda7103a02142d31363937383430313332313833343638303037000000000bebc20013343138353935353634383832353636383630" +
      "380000000000000000fda722fd03d300030005fdd824bfaf6c09039d5f8cc720c387e5c303d0205c27da418e690e62577b5e9c896a" +
      "ed76d91fe5df45b1082ee2c6367439f8120f1294d28658f1f4511319ba3da57e5f64cf8beaab25b9f3b9d15c4344cf6600dee69565" +
      "30d05923f219f3bab3b1960b61fdd8225b000195c368ccd9b6b45755bbd11e58f1376b78657f4d159a683350a9ab2cf7da10f40000" +
      "0000fdd8062b0002142d3136393738343031333231383334363830303713343138353935353634383832353636383630380564756d" +
      "6d79fdd824bf4ee2503496cb46026da419c5cef4d2647531f7b3356b0736821e2f73162f6b01d0879369762fa0eedb112a1a02e36f" +
      "86e035c3137b7ef5ba253f810464bde41165219eae4eec1d880e00d04c6d48cb6912b644c7a6b1094e7d82b36cfdce0f71fdd8225b" +
      "00012808ee563361556fb9a4949b278c27c27056191102db5e9a977c5c82d623871d00000000fdd8062b0002142d31363937383430" +
      "31333231383334363830303713343138353935353634383832353636383630380564756d6d79fdd824bf576589cd0d8e7fe996b049" +
      "d180d002a0c75b510960847478225eba4e39f13284edb45c5f035998dfc8166f64b8b0c159dc3d909a742d0422148f85cb3fe8f016" +
      "03433d626590b30351ceb2f0eae0b150b16d044f886c980f78b6c00eaa048979fdd8225b0001891f1c695bb10f4769608b36e703e6" +
      "78428f36cdb2611e4e2fa70ad56f37532900000000fdd8062b0002142d313639373834303133323138333436383030371334313835" +
      "3935353634383832353636383630380564756d6d79fdd824bfcee4dfa15d24db06fc145218d33df368198e426e876a98d351d83dc0" +
      "04b8b428aec0674493b0e765f2ead36dd575cb24e37d791e3e18c69ed28a0865b169444ad0861afa1b22c38c465f0acbd140ce3de4" +
      "85c22247164fad7daf7162b44d4901fdd8225b00013f340ca103eca4a6a1c49ccf8319ba9fbc6d9f31c7592586e87e3e7f0cb74f30" +
      "00000000fdd8062b0002142d3136393738343031333231383334363830303713343138353935353634383832353636383630380564" +
      "756d6d79fdd824bfdec152276ec26f0cbfb49e55f40e3a11ff1e305dde9481cc94832141a8102d9c861481796bf65b2205d75a1bd1" +
      "62be4ef2e4f1ae01e22ce6318743b4b4cf628c4c19449df7997504313270546943decf3b71ffc3068e8a5c80320c8b91e62159fdd8" +
      "225b0001ef88acc46190bb105a15827333e212b4f1bf5e437c8ae1f638a65a9d8ff0126000000000fdd8062b0002142d3136393738" +
      "343031333231383334363830303713343138353935353634383832353636383630380564756d6d79036dc06b5ba0d4957f38e9cc28" +
      "859e6ea88676c4453441e866605eed63181c164b001600149b1589d0bf8635c1f5662036200a4e1454966449000000000000000000" +
      "00000005f5e1000002fda7143f0000000000000000002902000000000100e1f505000000001600141c50c646c3818f6f4d0229715e" +
      "ae262ae4be69340000000000000000fffffffd006b0000fda7144b0000000000000001003502000000000100e1f505000000002200" +
      "20ddcc4b14b4fe91e90e0b9afb090feb892f4f4e6268c5a342e6f48715bc2303e10000000000000000fffffffd0095000000160014" +
      "2fc55cee805d4112a653e18d84cb405cb188b83000000000000000010000000000000000000000000000000a6320d4106320d411"

  val offer: LnMessage[DLCOfferTLV] = LnMessage(DLCOfferTLV.fromHex(str))

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

  it must "receive dlc node updates" in { serverWithBitcoind =>
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

    val peerAddr =
      InetSocketAddress.createUnresolved("127.0.0.1", NetworkUtil.randomPort())

    exec(AcceptDLC(offer = offer,
                   externalPayoutAddressOpt = None,
                   externalChangeAddressOpt = None,
                   peerAddr = peerAddr),
         cliConfig)

    for {
      _ <- AkkaUtil.nonBlockingSleep(500.millis)
      _ = promise.success(None)
      notifications <- walletNotificationsF
    } yield {
      assert(notifications.contains(DLCNodeConnectionInitiated(peerAddr)))
      assert(notifications.contains(DLCNodeConnectionFailed(peerAddr)))
      assert(
        notifications.contains(
          DLCAcceptFailed((offer.tlv.tempContractId, "Connection refused"))))
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
