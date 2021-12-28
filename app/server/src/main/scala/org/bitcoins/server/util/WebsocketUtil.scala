package org.bitcoins.server.util

import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.stream.scaladsl.SourceQueueWithComplete
import org.bitcoins.commons.jsonmodels.ws.{WalletNotification, WalletWsType}
import org.bitcoins.commons.serializers.WsPicklers
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.protocol.dlc.models.DLCStatus
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.dlc.wallet.{DLCWalletCallbacks, OnDLCStateChange}
import org.bitcoins.wallet.{
  OnBlockProcessed,
  OnNewAddressGenerated,
  OnReservedUtxos,
  OnTransactionBroadcast,
  OnTransactionProcessed,
  WalletCallbacks
}

import scala.concurrent.{ExecutionContext, Future}

object WebsocketUtil {

  /** Builds websocket callbacks for the wallet */
  def buildWalletCallbacks(
      walletQueue: SourceQueueWithComplete[Message],
      chainApi: ChainApi)(implicit ec: ExecutionContext): WalletCallbacks = {
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

    val onBlockProcessed: OnBlockProcessed = { block =>
      val resultF =
        ChainUtil.getBlockHeaderResult(block.blockHeader.hashBE, chainApi)
      val f = for {
        result <- resultF
        notification =
          WalletNotification.BlockProcessedNotification(result)
        notificationJson =
          upickle.default.writeJs(notification)(
            WsPicklers.blockProcessedPickler)
        msg = TextMessage.Strict(notificationJson.toString())
        _ <- walletQueue.offer(msg)
      } yield {
        ()
      }

      f
    }

    WalletCallbacks(
      onTransactionProcessed = Vector(onTxProcessed),
      onNewAddressGenerated = Vector(onAddressCreated),
      onReservedUtxos = Vector(onReservedUtxo),
      onTransactionBroadcast = Vector(onTxBroadcast),
      onBlockProcessed = Vector(onBlockProcessed)
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
          WalletWsType.BlockProcessed | WalletWsType.DLCStateChange) =>
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
