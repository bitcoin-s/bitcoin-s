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
import org.bitcoins.commons.jsonmodels.ws.{WalletNotification, WalletWsType}
import org.bitcoins.commons.jsonmodels.ws.WalletNotification.{
  BlockProcessedNotification,
  NewAddressNotification,
  TxBroadcastNotification,
  TxProcessedNotification
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

  def buildReq(conf: BitcoinSAppConfig): WebSocketRequest = {
    WebSocketRequest(s"ws://localhost:${conf.wsPort}/events")
  }

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

      val req = buildReq(server.conf)
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
      val cliConfig = Config(rpcPortOpt = Some(server.conf.rpcPort))
      val f: Flow[
        Message,
        Message,
        (Future[Seq[WalletNotification[_]]], Promise[Option[Message]])] = {
        Flow
          .fromSinkAndSourceCoupledMat(sink, Source.maybe[Message])(Keep.both)
      }

      val req = buildReq(server.conf)
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
      val cliConfig = Config(rpcPortOpt = Some(server.conf.rpcPort))
      val f: Flow[
        Message,
        Message,
        (Future[Seq[WalletNotification[_]]], Promise[Option[Message]])] = {
        Flow
          .fromSinkAndSourceCoupledMat(sink, Source.maybe[Message])(Keep.both)
      }

      val req = buildReq(server.conf)
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
        _ <- AkkaUtil.nonBlockingSleep(500.millis)
        _ = promise.success(None)
        notifications <- notificationsF
      } yield {
        assert(notifications.exists(_ == TxProcessedNotification(expectedTx)))
      }
  }

  it must "receive updates when a block is processed" in { serverWithBitcoind =>
    val ServerWithBitcoind(bitcoind, server) = serverWithBitcoind
    val cliConfig = Config(rpcPortOpt = Some(server.conf.rpcPort))
    val f: Flow[
      Message,
      Message,
      (Future[Seq[WalletNotification[_]]], Promise[Option[Message]])] = {
      Flow
        .fromSinkAndSourceCoupledMat(sink, Source.maybe[Message])(Keep.both)
    }

    val req = buildReq(server.conf)
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
      hashes <- bitcoind.generateToAddress(1, address)
      cmd = CliCommand.GetBlockHeader(hash = hashes.head)
      getBlockHeaderResultStr = ConsoleCli.exec(cmd, cliConfig)
      getBlockHeaderResult = upickle.default.read(getBlockHeaderResultStr.get)(
        Picklers.getBlockHeaderResultPickler)
      block <- bitcoind.getBlockRaw(hashes.head)
      wallet <- server.walletConf.createHDWallet(bitcoind, bitcoind, bitcoind)
      _ <- wallet.processBlock(block)
      _ <- AkkaUtil.nonBlockingSleep(500.millis)
      _ = promise.success(None)
      notifications <- notificationsF
    } yield {
      assert(
        notifications.exists(
          _ == BlockProcessedNotification(getBlockHeaderResult)))
    }
  }

  it must "get notifications for reserving and unreserving utxos" in {
    serverWithBitcoind =>
      val ServerWithBitcoind(_, server) = serverWithBitcoind
      val cliConfig = Config(rpcPortOpt = Some(server.conf.rpcPort))

      val f: Flow[
        Message,
        Message,
        (Future[Seq[WalletNotification[_]]], Promise[Option[Message]])] = {
        Flow
          .fromSinkAndSourceCoupledMat(sink, Source.maybe[Message])(Keep.both)
      }

      val req = buildReq(server.conf)
      val tuple: (
          Future[WebSocketUpgradeResponse],
          (Future[Seq[WalletNotification[_]]], Promise[Option[Message]])) = {
        Http()
          .singleWebSocketRequest(req, f)
      }

      val notificationsF: Future[Seq[WalletNotification[_]]] = tuple._2._1
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
}
