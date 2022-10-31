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
import org.bitcoins.core.protocol.tlv.{DLCOfferTLV, LnMessage, LnMessageFactory}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.DoubleSha256DigestBE
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
    "a71a000000010006226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910fb13ec60d0dd5d30d06708a2269230f9381e56ca128aeabd04dbb0ef94bffe36a000000000011e1a30000020357494e0000000011e1a300044c4f5345000000000000000000b8b3e935aef13d62b7c14cc59e67be0d518a9842614791769052495fb7ad2db44a2e099a5fa15d081cfe44abda181c4f34b886f03b347669dbd1c897b5e18e430766dad8d170c3011c3a13f363da9a3e237d52c7b75d2e9c69dc1f3789313b300b6f7261636c655f6e616d65126f7261636c655f6465736372697074696f6e000000000766dad8d170c3011c3a13f363da9a3e237d52c7b75d2e9c69dc1f3789313b3001281eff27a31917b748f9a47bea5341720a3ab4a8ccdeb9955c9e5845167dd6a20d628c085989a73a37102cc344190fde5ab58858423c53faab7ab370dfe023999037c9c6e975a919f4b32eb2fa2b559195b8f5dbb8d4726f24b7fe8f6d8734fe01e512af9e7bed34a2376a4fa120c1a2ed7c65fc2d2ad53fc3865c62d651fa351e9700dcf8963a8fde852a67487686f4bee59647af10b4fd8798f53dc7552d161998d0936fd063df9209e03027389817c4ef2151cff8485855158b29a8ad32d90ee91e25a1662f6b4c34d8af1191ce49ed3b40b5b80a6706abdf3e28540a9ecce5000000000000020357494e044c4f53450564756d6d79029463b53fa41300bc75ff8518ba72db6447bb08ceb4a45371a16156b4a7dfc62d0016001442d8b57a960c708a3dd6d0735310fb92b8294ace7e1246eef8a2c01c0000000008f0d1800272b3c84fa08bd7cd52010000000130290a1b739258a838a51fd5eb44ba7eb29b60cc0182288b6b83baed26e2abe4c67a3a0b00ffffffff0100e1f50500000000160014f6bc4efa608307e8a3347b79935dd6d9fabf47d50000000000000000fffffffd006b000024589e926241cb4a52010000000116912986c232eea8e638c0bc667315d6ba73f7be79a429c7c7ca30c9a464af4b4718eddb00ffffffff0100a3e111000000001600149ba3db05ed982955af66b7349156f45b1e2cf0780000000000000000fffffffd006b000000160014064a62b0f262d3abc457bca6beb20141c7e54b081360f5eca5ee03e1e1f9afc09a0168fe00000000000000030000000000000001"

  val offer: LnMessage[DLCOfferTLV] = LnMessageFactory(DLCOfferTLV).fromHex(str)

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
        "a71a000000010006226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910fb821ef0c9d21e6c331b48ae9c8234131b51d8ca7491fbc3a84b388f36bf89eda00000000000bebc20000040161000000000bebc200016200000000000000000163000000000bebc2000164000000000000000000c71fa987ff411d642ccda7ae0ef00aeac06188043132f7c4bd7d435b5530e4671cdda82b6ed09021e2960f453df1a7bc10da602762aa3acb0a1ae13c4dad232d5c41cd879a7c8ea413f394c295135ff90996b0f2d47f05ec13d19a1fd594f5e00a4d6f636b4f7261636c650b6d6f636b206f7261636c6500000001283163d44dc3027ede3f29ca380d72d472d4232bfa85bcbd1c55c4182992067901724ca061831947de6d27726beae3e88fc86750914f6e6562d327b9da5dbbb0ae53142270506b7da760bbb02b8eaf6c1b14eeb8549501af5e9a9f52eef75710cd9a91a6038393887dbf6674ca22503113156c0f4ed4b851dca26723cc6d2f45bc019be2a2d6df9a2fa53b118c4fbfe7b074d9c5fc50a97f76d9fa35e45b3dd9b7b8c7e2e58443ac6841d081620a86aec2e7e7eff783a88d3d9ce50d658e38a4118c89f9ef2e6f5520c7f1e01cc0344648a9ca149c7d6791774ee0d2b8d5a4be249ee4c2f7ccbb3ae7620d35d41df5a7f57c66babd32afa1e3fbe54c85f3805c063b0060bf0bb0000401610162016301640454657374026f58c4b58d19857d9cfbbd79a79ac985c20c218961eded79a2d6014a8b5facbe001600144532e712b69a6a79c1435fcb16f231932fd5215800ad6405d2aaf18c0000000005f5e10001db260902a1853bdaa8020000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff03520101ffffffff0200f2052a01000000160014d1d5eb74c29e2367a8eee45d228d60aab35feab40000000000000000266a24aa21a9ede2f61c3f71d1defd3fa999dfa36953755c690689799962b48bebd836974e8cf9012000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffff006b000000160014bf3bc3658d7dcaef02bd3ffdb8fc9b145fb780dc2e2e70325cfae83968acab46bb30e5c7000000000000000260bf0bb060c84630"

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

    val setupF = notificationsF._1
    val walletNotificationsF: Future[Seq[WsNotification[_]]] =
      notificationsF._2._1

    val promise: Promise[Option[Message]] = notificationsF._2._2

    val peerAddr =
      InetSocketAddress.createUnresolved("127.0.0.1", NetworkUtil.randomPort())

    val acceptMsg = {
      AcceptDLC(offer = offer,
                externalPayoutAddressOpt = None,
                externalChangeAddressOpt = None,
                peerAddr = peerAddr)
    }
    for {
      _ <- setupF
      _ = ConsoleCli.exec(acceptMsg, cliConfig)
      _ <- AkkaUtil.nonBlockingSleep(500.millis)
      _ = promise.success(None)
      notifications <- walletNotificationsF
    } yield {
      assert(notifications.exists(_ == DLCNodeConnectionInitiated(peerAddr)))
      assert(notifications.exists(_ == DLCNodeConnectionFailed(peerAddr)))
      assert(notifications.exists(n =>
        n match {
          case DLCAcceptFailed((id, error)) =>
            id == offer.tlv.tempContractId && error.startsWith(
              "Connection refused")
          case _ => false
        }))
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
