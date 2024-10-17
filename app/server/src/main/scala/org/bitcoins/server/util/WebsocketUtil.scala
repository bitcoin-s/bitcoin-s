package org.bitcoins.server.util

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.scaladsl.SourceQueueWithComplete
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.{
  ChainCallbacks,
  OnBlockHeaderConnected,
  OnCompactFilterConnected,
  OnCompactFilterHeaderConnected,
  OnSyncFlagChanged
}
import org.bitcoins.commons.jsonmodels.ws.ChainNotification.{
  BlockProcessedNotification,
  CompactFilterHeaderProcessedNotification,
  CompactFilterProcessedNotification
}
import org.bitcoins.commons.jsonmodels.ws.TorNotification.TorStartedNotification
import org.bitcoins.commons.jsonmodels.ws.{
  ChainNotification,
  DLCNodeNotification,
  WalletNotification,
  WalletWsType,
  WsNotification
}
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.db.{CompactFilterDb, CompactFilterHeaderDb}
import org.bitcoins.core.api.dlc.wallet.db.IncomingDLCOfferDb
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.protocol.dlc.models.DLCStatus
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.{DoubleSha256DigestBE, Sha256Digest}
import org.bitcoins.dlc.node.{
  DLCNodeCallbacks,
  OnAcceptFailed,
  OnAcceptSucceed,
  OnOfferSendFailed,
  OnOfferSendSucceed,
  OnPeerConnectionEstablished,
  OnPeerConnectionFailed,
  OnPeerConnectionInitiated,
  OnSignFailed,
  OnSignSucceed
}
import org.bitcoins.dlc.wallet.callback.{
  DLCWalletCallbackStreamManager,
  DLCWalletCallbacks,
  OnDLCOfferAdd,
  OnDLCOfferRemove,
  OnDLCStateChange
}
import org.bitcoins.tor.{OnTorStarted, TorCallbacks}
import org.bitcoins.wallet.callback.{
  OnFeeRateChanged,
  OnNewAddressGenerated,
  OnRescanComplete,
  OnReservedUtxos,
  OnTransactionBroadcast,
  OnTransactionProcessed,
  WalletCallbackStreamManager,
  WalletCallbacks
}

import scala.concurrent.{ExecutionContext, Future}

object WebsocketUtil extends BitcoinSLogger {

  private def sendHeadersToWs(
      notifications: Vector[ChainNotification[?]],
      queue: SourceQueueWithComplete[WsNotification[?]]
  )(implicit ec: ExecutionContext): Future[Unit] = {
    for {
      _ <- FutureUtil.sequentially(notifications) { case msg =>
        val x: Future[Unit] = queue
          .offer(msg)
          .map(_ => ())
        x
      }
    } yield ()
  }

  def buildChainCallbacks(
      queue: SourceQueueWithComplete[WsNotification[?]],
      chainApi: ChainApi
  )(implicit
      ec: ExecutionContext,
      chainAppConfig: ChainAppConfig
  ): ChainCallbacks = {
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
            // only emit the last header so that we don't overwhelm the UI
            for {
              results <- resultsF
              notification = BlockProcessedNotification(results.last)
              _ <- sendHeadersToWs(Vector(notification), queue)
            } yield ()
          } else {
            val f = for {
              results <- resultsF
              notifications = results.map(BlockProcessedNotification(_))
              _ <- sendHeadersToWs(notifications, queue)
            } yield {
              ()
            }
            f
          }
        }
    }

    val onCompactFilterHeaderProcessed: OnCompactFilterHeaderConnected = {
      case filterHeaders: Vector[CompactFilterHeaderDb] =>
        val isIBDF = chainApi.isIBD()
        val emitBlockProccessedWhileIBDOnGoing =
          chainAppConfig.ibdBlockProcessedEvents
        isIBDF.flatMap { isIBD =>
          if (
            isIBD && !emitBlockProccessedWhileIBDOnGoing && filterHeaders.nonEmpty
          ) {
            val notifications =
              CompactFilterHeaderProcessedNotification(filterHeaders.last)
            sendHeadersToWs(Vector(notifications), queue)
          } else {
            val notifications =
              filterHeaders.map(CompactFilterHeaderProcessedNotification(_))
            sendHeadersToWs(notifications, queue)
          }
        }
    }

    val onCompactFilterProcessed: OnCompactFilterConnected = {
      case filters: Vector[CompactFilterDb] =>
        val isIBDF = chainApi.isIBD()
        val emitBlockProccessedWhileIBDOnGoing =
          chainAppConfig.ibdBlockProcessedEvents
        isIBDF.flatMap { isIBD =>
          if (
            isIBD && !emitBlockProccessedWhileIBDOnGoing && filters.nonEmpty
          ) {
            val notifications = CompactFilterProcessedNotification(filters.last)
            sendHeadersToWs(Vector(notifications), queue)
          } else {
            val notifications =
              filters.map(CompactFilterProcessedNotification(_))
            sendHeadersToWs(notifications, queue)
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
      ChainCallbacks.onOnSyncFlagChanged(onSyncFlagChanged) +
      ChainCallbacks.onCompactFilterHeaderConnected(
        onCompactFilterHeaderProcessed
      ) +
      ChainCallbacks.onCompactFilterConnected(onCompactFilterProcessed)
  }

  /** Builds websocket callbacks for the wallet */
  def buildWalletCallbacks(
      walletQueue: SourceQueueWithComplete[WsNotification[?]],
      walletName: String
  )(implicit system: ActorSystem): WalletCallbackStreamManager = {
    import system.dispatcher
    val onAddressCreated: OnNewAddressGenerated = { addr =>
      val notification = WalletNotification.NewAddressNotification(addr)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    val onTxProcessed: OnTransactionProcessed = { tx =>
      buildTxNotification(
        wsType = WalletWsType.TxProcessed,
        tx = tx,
        walletQueue = walletQueue
      )
    }

    val onTxBroadcast: OnTransactionBroadcast = { tx =>
      buildTxNotification(
        wsType = WalletWsType.TxBroadcast,
        tx = tx,
        walletQueue = walletQueue
      )
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

    val callbacks = WalletCallbacks(
      onTransactionProcessed = Vector(onTxProcessed),
      onTransactionBroadcast = Vector(onTxBroadcast),
      onReservedUtxos = Vector(onReservedUtxo),
      onNewAddressGenerated = Vector(onAddressCreated),
      onBlockProcessed = Vector.empty,
      onRescanComplete = Vector(onRescanComplete),
      onFeeRateChanged = Vector(onFeeRate)
    )

    WalletCallbackStreamManager(callbacks = callbacks)
  }

  def buildTorCallbacks(
      queue: SourceQueueWithComplete[WsNotification[?]]
  )(implicit ec: ExecutionContext): TorCallbacks = {
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
      walletQueue: SourceQueueWithComplete[WsNotification[?]]
  )(implicit ec: ExecutionContext): Future[Unit] = {
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
      walletQueue: SourceQueueWithComplete[WsNotification[?]]
  )(implicit system: ActorSystem): DLCWalletCallbackStreamManager = {
    import system.dispatcher
    val onStateChange: OnDLCStateChange = { (status: DLCStatus) =>
      val notification = WalletNotification.DLCStateChangeNotification(status)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    val onOfferAdd: OnDLCOfferAdd = { (offerDb: IncomingDLCOfferDb) =>
      val notification = WalletNotification.DLCOfferAddNotification(offerDb)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    val onOfferRemove: OnDLCOfferRemove = { (offerHash: Sha256Digest) =>
      val notification =
        WalletNotification.DLCOfferRemoveNotification(offerHash)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    import DLCWalletCallbacks._

    val callbacks = onDLCStateChange(onStateChange) + onDLCOfferAdd(
      onOfferAdd
    ) + onDLCOfferRemove(onOfferRemove)

    DLCWalletCallbackStreamManager(callbacks)
  }

  def buildDLCNodeCallbacks(
      walletQueue: SourceQueueWithComplete[WsNotification[?]]
  )(implicit ec: ExecutionContext): DLCNodeCallbacks = {

    val onConnectionInitiated: OnPeerConnectionInitiated = { payload =>
      val notification =
        DLCNodeNotification.DLCNodeConnectionInitiated(payload)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    val onConnectionEstablished: OnPeerConnectionEstablished = { payload =>
      val notification =
        DLCNodeNotification.DLCNodeConnectionEstablished(payload)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    val onConnectionFailed: OnPeerConnectionFailed = { payload =>
      val notification = DLCNodeNotification.DLCNodeConnectionFailed(payload)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    val onAcceptSucceed: OnAcceptSucceed = { payload =>
      val notification = DLCNodeNotification.DLCAcceptSucceed(payload)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    val onAcceptFailed: OnAcceptFailed = { payload =>
      val notification = DLCNodeNotification.DLCAcceptFailed(payload)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    val onOfferSendSucceed: OnOfferSendSucceed = { payload =>
      val notification = DLCNodeNotification.DLCOfferSendSucceed(payload)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    val onOfferSendFailed: OnOfferSendFailed = { payload =>
      val notification = DLCNodeNotification.DLCOfferSendFailed(payload)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    val onSignSucceed: OnSignSucceed = { payload =>
      val notification = DLCNodeNotification.DLCSignSucceed(payload)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    val onSignFailed: OnSignFailed = { payload =>
      val notification = DLCNodeNotification.DLCSignFailed(payload)
      val offerF = walletQueue.offer(notification)
      offerF.map(_ => ())
    }

    DLCNodeCallbacks(
      onPeerConnectionInitiated = Vector(onConnectionInitiated),
      onPeerConnectionEstablished = Vector(onConnectionEstablished),
      onPeerConnectionFailed = Vector(onConnectionFailed),
      onOfferSendSucceed = Vector(onOfferSendSucceed),
      onOfferSendFailed = Vector(onOfferSendFailed),
      onAcceptSucceed = Vector(onAcceptSucceed),
      onAcceptFailed = Vector(onAcceptFailed),
      onSignSucceed = Vector(onSignSucceed),
      onSignFailed = Vector(onSignFailed)
    )
  }
}
