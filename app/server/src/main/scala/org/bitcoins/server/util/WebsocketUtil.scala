package org.bitcoins.server.util

import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.stream.scaladsl.SourceQueueWithComplete
import grizzled.slf4j.Logging
import org.bitcoins.chain.{ChainCallbacks, OnBlockHeaderConnected}
import org.bitcoins.commons.jsonmodels.ws.{
  ChainNotification,
  WalletNotification,
  WalletWsType
}
import org.bitcoins.commons.serializers.WsPicklers
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.protocol.dlc.models.DLCStatus
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.dlc.wallet.{DLCWalletCallbacks, OnDLCStateChange}
import org.bitcoins.wallet._

import scala.concurrent.{ExecutionContext, Future}

object WebsocketUtil extends Logging {

  def buildChainCallbacks(
      queue: SourceQueueWithComplete[Message],
      chainApi: ChainApi)(implicit ec: ExecutionContext): ChainCallbacks = {
    val onBlockProcessed: OnBlockHeaderConnected = {
      case headersWithHeight: Vector[(Int, BlockHeader)] =>
        val hashes: Vector[DoubleSha256DigestBE] =
          headersWithHeight.map(_._2.hashBE)
        val resultsF =
          ChainUtil.getBlockHeaderResult(hashes, chainApi)
        val f = for {
          results <- resultsF
          notifications =
            results.map(result =>
              ChainNotification.BlockProcessedNotification(result))
          notificationsJson = notifications.map { notification =>
            upickle.default.writeJs(notification)(
              WsPicklers.blockProcessedPickler)
          }

          msgs = notificationsJson.map(n => TextMessage.Strict(n.toString()))
          _ <- FutureUtil.sequentially(msgs) { case msg =>
            val x: Future[Unit] = queue
              .offer(msg)
              .map(_ => ())
            x
          }
        } yield {
          ()
        }
        f
    }

    ChainCallbacks.onBlockHeaderConnected(onBlockProcessed)
  }

  /** Builds websocket callbacks for the wallet */
  def buildWalletCallbacks(walletQueue: SourceQueueWithComplete[Message])(
      implicit ec: ExecutionContext): WalletCallbacks = {
    val onAddressCreated: OnNewAddressGenerated = { addr =>
      val notification = WalletNotification.NewAddressNotification(addr)
      val json =
        upickle.default.writeJs(notification)(WsPicklers.newAddressPickler)
      val msg = TextMessage.Strict(json.toString())
      val offerF = walletQueue.offer(msg)
      offerF.map(_ => ())
    }

    val onTxProcessed: OnTransactionProcessed = { tx =>
      buildTxNotification(wsType = WalletWsType.TxProcessed,
                          tx = tx,
                          walletQueue = walletQueue)
    }

    val onTxBroadcast: OnTransactionBroadcast = { tx =>
      buildTxNotification(wsType = WalletWsType.TxBroadcast,
                          tx = tx,
                          walletQueue = walletQueue)
    }

    val onReservedUtxo: OnReservedUtxos = { utxos =>
      val notification =
        WalletNotification.ReservedUtxosNotification(utxos)
      val notificationJson =
        upickle.default.writeJs(notification)(WsPicklers.reservedUtxosPickler)
      val msg = TextMessage.Strict(notificationJson.toString())
      val offerF = walletQueue.offer(msg)
      offerF.map(_ => ())
    }

    WalletCallbacks(
      onTransactionProcessed = Vector(onTxProcessed),
      onNewAddressGenerated = Vector(onAddressCreated),
      onReservedUtxos = Vector(onReservedUtxo),
      onTransactionBroadcast = Vector(onTxBroadcast)
    )
  }

  private def buildTxNotification(
      wsType: WalletWsType,
      tx: Transaction,
      walletQueue: SourceQueueWithComplete[Message])(implicit
      ec: ExecutionContext): Future[Unit] = {
    val json = wsType match {
      case WalletWsType.TxProcessed =>
        val notification = WalletNotification.TxProcessedNotification(tx)
        upickle.default.writeJs(notification)(WsPicklers.txProcessedPickler)
      case WalletWsType.TxBroadcast =>
        val notification = WalletNotification.TxBroadcastNotification(tx)
        upickle.default.writeJs(notification)(WsPicklers.txBroadcastPickler)
      case x @ (WalletWsType.NewAddress | WalletWsType.ReservedUtxos |
          WalletWsType.DLCStateChange) =>
        sys.error(s"Cannot build tx notification for $x")
    }

    val msg = TextMessage.Strict(json.toString())
    val offerF = walletQueue.offer(msg)
    offerF.map(_ => ())
  }

  def buildDLCWalletCallbacks(walletQueue: SourceQueueWithComplete[Message])(
      implicit ec: ExecutionContext): DLCWalletCallbacks = {
    val onStateChange: OnDLCStateChange = { status: DLCStatus =>
      val notification = WalletNotification.DLCStateChangeNotification(status)
      val json =
        upickle.default.writeJs(notification)(WsPicklers.dlcStateChangePickler)
      val msg = TextMessage.Strict(json.toString())
      val offerF = walletQueue.offer(msg)
      offerF.map(_ => ())
    }

    DLCWalletCallbacks.onDLCStateChange(onStateChange)
  }
}
