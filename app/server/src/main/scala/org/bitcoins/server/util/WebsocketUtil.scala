package org.bitcoins.server.util

import akka.stream.scaladsl.SourceQueueWithComplete
import grizzled.slf4j.Logging
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.{
  ChainCallbacks,
  OnBlockHeaderConnected,
  OnSyncFlagChanged
}
import org.bitcoins.commons.jsonmodels.ws.TorNotification.TorStartedNotification
import org.bitcoins.commons.jsonmodels.ws.{
  ChainNotification,
  WalletNotification,
  WalletWsType,
  WsNotification
}
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.dlc.wallet.db.IncomingDLCOfferDb
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.protocol.dlc.models.DLCStatus
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.{DoubleSha256DigestBE, Sha256Digest}
import org.bitcoins.dlc.wallet.{
  DLCWalletCallbacks,
  OnDLCOfferAdd,
  OnDLCOfferRemove,
  OnDLCStateChange
}
import org.bitcoins.tor.{OnTorStarted, TorCallbacks}
import org.bitcoins.wallet._

import scala.concurrent.{ExecutionContext, Future}

object WebsocketUtil extends Logging {

  def buildChainCallbacks(
      queue: SourceQueueWithComplete[WsNotification[_]],
      chainApi: ChainApi)(implicit
      ec: ExecutionContext,
      chainAppConfig: ChainAppConfig): ChainCallbacks = {
    val onBlockProcessed: OnBlockHeaderConnected = {
      case headersWithHeight: Vector[(Int, BlockHeader)] =>
        val hashes: Vector[DoubleSha256DigestBE] =
          headersWithHeight.map(_._2.hashBE)
        val resultsF =
          ChainUtil.getBlockHeaderResult(hashes, chainApi)

        val isIBDF = chainApi.isIBD()
        val emitBlockProccessedWhileIBDOnGoing =
          chainAppConfig.ibdBlockProcessedEvents
        isIBDF.flatMap { isIBD =>
          if (isIBD && !emitBlockProccessedWhileIBDOnGoing) {
            //do nothing, don't emit events until IBD is complete
            Future.unit
          } else {
            val f = for {
              results <- resultsF
              notifications =
                results.map(result =>
                  ChainNotification.BlockProcessedNotification(result))
              _ <- FutureUtil.sequentially(notifications) { case msg =>
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
        }
    }

    val onSyncFlagChanged: OnSyncFlagChanged = { syncing =>
      val notification = ChainNotification.SyncFlagChangedNotification(syncing)
      for {
        _ <- queue.offer(notification)
      } yield ()
    }

    ChainCallbacks.onBlockHeaderConnected(onBlockProcessed) +
      ChainCallbacks.onOnSyncFlagChanged(onSyncFlagChanged)
  }

  /** Builds websocket callbacks for the wallet */
  def buildWalletCallbacks(
      walletQueue: SourceQueueWithComplete[WsNotification[_]],
      walletName: String)(implicit ec: ExecutionContext): WalletCallbacks = {
    val onAddressCreated: OnNewAddressGenerated = { addr =>
      val notification = WalletNotification.NewAddressNotification(addr)
      val offerF = walletQueue.offer(notification)
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
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    val onRescanComplete: OnRescanComplete = { _ =>
      val notification = WalletNotification.RescanComplete(walletName)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    val onFeeRate: OnFeeRateChanged = { feeRate =>
      val notification = WalletNotification.FeeRateChange(feeRate)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    WalletCallbacks(
      onTransactionProcessed = Vector(onTxProcessed),
      onTransactionBroadcast = Vector(onTxBroadcast),
      onReservedUtxos = Vector(onReservedUtxo),
      onNewAddressGenerated = Vector(onAddressCreated),
      onBlockProcessed = Vector.empty,
      onRescanComplete = Vector(onRescanComplete),
      onFeeRateChanged = Vector(onFeeRate)
    )
  }

  def buildTorCallbacks(queue: SourceQueueWithComplete[WsNotification[_]])(
      implicit ec: ExecutionContext): TorCallbacks = {
    val onTorStarted: OnTorStarted = { _ =>
      val notification = TorStartedNotification
      val offerF = queue.offer(notification)
      offerF.map(_ => ())
    }

    TorCallbacks(onTorStarted)
  }

  private def buildTxNotification(
      wsType: WalletWsType,
      tx: Transaction,
      walletQueue: SourceQueueWithComplete[WsNotification[_]])(implicit
      ec: ExecutionContext): Future[Unit] = {
    val notification = wsType match {
      case WalletWsType.TxProcessed =>
        WalletNotification.TxProcessedNotification(tx)
      case WalletWsType.TxBroadcast =>
        WalletNotification.TxBroadcastNotification(tx)
      case x @ (WalletWsType.NewAddress | WalletWsType.ReservedUtxos |
          WalletWsType.DLCStateChange | WalletWsType.DLCOfferAdd |
          WalletWsType.DLCOfferRemove | WalletWsType.RescanComplete |
          WalletWsType.FeeRateChange) =>
        sys.error(s"Cannot build tx notification for $x")
    }

    val offerF = walletQueue.offer(notification)
    offerF.map(_ => ())
  }

  def buildDLCWalletCallbacks(
      walletQueue: SourceQueueWithComplete[WsNotification[_]])(implicit
      ec: ExecutionContext): DLCWalletCallbacks = {
    val onStateChange: OnDLCStateChange = { status: DLCStatus =>
      val notification = WalletNotification.DLCStateChangeNotification(status)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    val onOfferAdd: OnDLCOfferAdd = { offerDb: IncomingDLCOfferDb =>
      val notification = WalletNotification.DLCOfferAddNotification(offerDb)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    val onOfferRemove: OnDLCOfferRemove = { offerHash: Sha256Digest =>
      val notification =
        WalletNotification.DLCOfferRemoveNotification(offerHash)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    import DLCWalletCallbacks._

    onDLCStateChange(onStateChange) + onDLCOfferAdd(
      onOfferAdd) + onDLCOfferRemove(onOfferRemove)
  }
}
