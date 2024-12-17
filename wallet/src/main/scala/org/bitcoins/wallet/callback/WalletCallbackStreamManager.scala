package org.bitcoins.wallet.callback

import org.apache.pekko.Done
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.{Attributes, OverflowStrategy}
import org.apache.pekko.stream.scaladsl.{
  Keep,
  Sink,
  Source,
  SourceQueueWithComplete
}
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.CallbackHandler
import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.core.wallet.fee.FeeUnit

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.Future

case class WalletCallbackStreamManager(
    callbacks: WalletCallbacks,
    overflowStrategy: OverflowStrategy = OverflowStrategy.backpressure,
    maxBufferSize: Int = 16
)(implicit system: ActorSystem)
    extends WalletCallbacks
    with StartStopAsync[Unit]
    with BitcoinSLogger {
  import system.dispatcher

  private val txProcessedQueueSource
      : Source[Transaction, SourceQueueWithComplete[Transaction]] = {
    Source
      .queue[Transaction](maxBufferSize, overflowStrategy)
      .log("wallet-txprocessingsource")
      .withAttributes(Attributes.name("wallet-txprocessingsource"))
  }

  private val txProcessedSink: Sink[Transaction, Future[Done]] = {
    Sink.foreachAsync(1) { case tx =>
      callbacks.executeOnTransactionProcessed(tx)
    }
  }

  private val (txProcessedQueue, txProcessedSinkF) = {
    matSourceAndQueue(txProcessedQueueSource, txProcessedSink)
  }

  private val txBroadcastQueueSource
      : Source[Transaction, SourceQueueWithComplete[Transaction]] = {
    Source
      .queue[Transaction](maxBufferSize, overflowStrategy)
      .log("wallet-txBroadcastQueueSource")
      .withAttributes(Attributes.name("wallet-txBroadcastQueueSource"))
  }

  private val txBroadcastSink: Sink[Transaction, Future[Done]] = {
    Sink.foreachAsync(1) { case tx =>
      callbacks.executeOnTransactionBroadcast(tx)
    }
  }

  private val (txBroadcastQueue, txBroadcastSinkF) = {
    matSourceAndQueue(txBroadcastQueueSource, txBroadcastSink)
  }

  private val onReservedUtxosSource
      : Source[Vector[
                 SpendingInfoDb
               ],
               SourceQueueWithComplete[Vector[SpendingInfoDb]]] = {
    Source
      .queue[Vector[
        SpendingInfoDb
      ]](maxBufferSize, overflowStrategy)
      .log("wallet-onReservedUtxosSource")
      .withAttributes(Attributes.name("wallet-onReservedUtxosSource"))
  }

  private val onReservedUtxosSink
      : Sink[Vector[SpendingInfoDb], Future[Done]] = {
    Sink.foreachAsync(1) { case utxos =>
      callbacks.executeOnReservedUtxos(utxos)
    }
  }

  private val (onReservedUtxosQueue, onReservedUtxosSinkF) = {
    matSourceAndQueue(onReservedUtxosSource, onReservedUtxosSink)
  }

  private val onAddressGeneratedSource
      : Source[BitcoinAddress, SourceQueueWithComplete[BitcoinAddress]] = {
    Source
      .queue[BitcoinAddress](maxBufferSize, overflowStrategy)
      .log("wallet-onAddressGeneratedSource")
      .withAttributes(Attributes.name("wallet-onAddressGeneratedSource"))
  }

  private val onAddressGeneratedSink: Sink[BitcoinAddress, Future[Done]] = {
    Sink.foreachAsync(1) { case addr =>
      callbacks.executeOnNewAddressGenerated(addr)
    }
  }

  private val (onAddressGeneratedQueue, onAddressGeneratedSinkF) = {
    matSourceAndQueue(onAddressGeneratedSource, onAddressGeneratedSink)
  }

  private val onBlockProcessedSource
      : Source[Block, SourceQueueWithComplete[Block]] = {
    Source
      .queue[Block](maxBufferSize, overflowStrategy)
      .log("wallet-onBlockProcessedSource")
      .withAttributes(Attributes.name("wallet-onBlockProcessedSource"))
  }

  private val onBockProcessedSink: Sink[Block, Future[Done]] = {
    Sink.foreachAsync(1) { case block =>
      callbacks.executeOnBlockProcessed(block)
    }
  }

  private val (onBlockProcessedQueue, onBlockProcessedSinkF) = {
    matSourceAndQueue(onBlockProcessedSource, onBockProcessedSink)
  }

  private val onRescanCompleteSource
      : Source[String, SourceQueueWithComplete[String]] = {
    Source
      .queue[String](maxBufferSize, overflowStrategy)
      .log("wallet-onRescanCompleteSource")
      .withAttributes(Attributes.name("wallet-onRescanCompleteSource"))
  }

  private val onRescanCompleteSink: Sink[String, Future[Done]] = {
    Sink.foreachAsync(1) { case str =>
      callbacks.executeOnRescanComplete(str)
    }
  }

  private val (onRescanCompleteQueue, onRescanCompleteSinkF) = {
    matSourceAndQueue(onRescanCompleteSource, onRescanCompleteSink)
  }

  private val onFeeChangeSource
      : Source[FeeUnit, SourceQueueWithComplete[FeeUnit]] = {
    Source
      .queue[FeeUnit](maxBufferSize, overflowStrategy)
      .log("wallet-onFeeChangeSource")
      .withAttributes(Attributes.name("wallet-onFeeChangeSource"))
  }

  private val onFeeChangeSink: Sink[FeeUnit, Future[Done]] = {
    Sink.foreachAsync(1) { case feeRate =>
      callbacks.executeOnFeeRateChanged(feeRate)
    }
  }

  private val (onFeeChangeSourceQueue, onFeeChangeSinkF) = {
    matSourceAndQueue(onFeeChangeSource, onFeeChangeSink)
  }

  override def start(): Future[Unit] = Future.unit

  private val isStopped: AtomicBoolean = new AtomicBoolean(false)

  override def stop(): Future[Unit] = {
    val start = System.currentTimeMillis()

    // can't complete a stream twice
    if (!isStopped.get()) {
      // complete all queues
      txProcessedQueue.complete()
      txBroadcastQueue.complete()
      onReservedUtxosQueue.complete()
      onAddressGeneratedQueue.complete()
      onBlockProcessedQueue.complete()
      onRescanCompleteQueue.complete()
      onFeeChangeSourceQueue.complete()
      isStopped.set(true)
    } else {
      logger.warn(
        s"Already stopped all queues associated with this NodeCallBackStreamManager"
      )
    }

    for {
      _ <- txProcessedSinkF
      _ <- txBroadcastSinkF
      _ <- onReservedUtxosSinkF
      _ <- onAddressGeneratedSinkF
      _ <- onBlockProcessedSinkF
      _ <- onRescanCompleteSinkF
      _ <- onFeeChangeSinkF
    } yield {
      logger.info(
        s"Done draining akka streams for WalletCallbackStreamManager, it took=${System
            .currentTimeMillis() - start}ms"
      )
      ()
    }
  }

  private def matSourceAndQueue[T](
      source: Source[T, SourceQueueWithComplete[T]],
      sink: Sink[T, Future[Done]]
  ): (SourceQueueWithComplete[T], Future[Done]) = {
    source
      .toMat(sink)(Keep.both)
      .run()
  }

  override def onTransactionProcessed
      : CallbackHandler[Transaction, OnTransactionProcessed] = {
    callbacks.onTransactionProcessed
  }

  override def onTransactionBroadcast
      : CallbackHandler[Transaction, OnTransactionBroadcast] = {
    callbacks.onTransactionBroadcast
  }

  override def onReservedUtxos
      : CallbackHandler[Vector[SpendingInfoDb], OnReservedUtxos] = {
    callbacks.onReservedUtxos
  }

  override def onNewAddressGenerated
      : CallbackHandler[BitcoinAddress, OnNewAddressGenerated] = {
    callbacks.onNewAddressGenerated
  }

  override def onBlockProcessed: CallbackHandler[Block, OnBlockProcessed] = {
    callbacks.onBlockProcessed
  }

  override def onRescanComplete: CallbackHandler[String, OnRescanComplete] = {
    callbacks.onRescanComplete
  }

  override def onFeeRateChanged: CallbackHandler[FeeUnit, OnFeeRateChanged] = {
    callbacks.onFeeRateChanged
  }

  override def +(other: WalletCallbacks): WalletCallbacks = {
    val newCallbacks = other.+(this)
    WalletCallbackStreamManager(newCallbacks)
  }
}
