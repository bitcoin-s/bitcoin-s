package org.bitcoins.dlc.wallet.callback

import akka.Done
import akka.actor.ActorSystem
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Keep, Sink, Source, SourceQueueWithComplete}
import grizzled.slf4j.{Logging}
import org.bitcoins.core.api.CallbackHandler
import org.bitcoins.core.api.dlc.wallet.db.IncomingDLCOfferDb
import org.bitcoins.core.protocol.dlc.models.DLCStatus
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.crypto.Sha256Digest

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{Future}

case class DLCWalletCallbackStreamManager(
    override val onStateChange: CallbackHandler[DLCStatus, OnDLCStateChange],
    override val onOfferAdd: CallbackHandler[IncomingDLCOfferDb, OnDLCOfferAdd],
    override val onOfferRemove: CallbackHandler[Sha256Digest, OnDLCOfferRemove],
    overflowStrategy: OverflowStrategy = OverflowStrategy.backpressure,
    maxBufferSize: Int = 16)(implicit system: ActorSystem)
    extends DLCWalletCallbacks(onStateChange, onOfferAdd, onOfferRemove)
    with StartStopAsync[Unit]
    with Logging {

  import system.dispatcher

  private val stateChangeSource: Source[
    DLCStatus,
    SourceQueueWithComplete[DLCStatus]] = {
    Source.queue(maxBufferSize, overflowStrategy)
  }

  private val stateChangeSink: Sink[DLCStatus, Future[Done]] = {
    Sink.foreachAsync(1) { case state =>
      onStateChange.execute(state)
    }
  }

  private val (stateChangeQueue, stateChangeSinkCompleteF) = {
    matSourceAndQueue(stateChangeSource, stateChangeSink)
  }

  private val offerAddSource: Source[
    IncomingDLCOfferDb,
    SourceQueueWithComplete[IncomingDLCOfferDb]] = {
    Source.queue(maxBufferSize, overflowStrategy)
  }

  private val offerAddSink: Sink[IncomingDLCOfferDb, Future[Done]] = {
    Sink.foreachAsync(1) { case offer =>
      onOfferAdd.execute(offer)
    }
  }

  private val (offerAddQueue, offerAddSinkCompleteF) = {
    matSourceAndQueue(offerAddSource, offerAddSink)
  }

  private val offerRemoveSource: Source[
    IncomingDLCOfferDb,
    SourceQueueWithComplete[IncomingDLCOfferDb]] = {
    Source.queue(maxBufferSize, overflowStrategy)
  }

  private val offerRemoveSink: Sink[IncomingDLCOfferDb, Future[Done]] = {
    Sink.foreachAsync(1) { case offer =>
      onOfferAdd.execute(offer)
    }
  }

  private val (offerRemoveQueue, offerRemoveSinkCompleteF) = {
    matSourceAndQueue(offerRemoveSource, offerRemoveSink)
  }

  override def +(other: DLCWalletCallbacks): DLCWalletCallbacks = {
    val newCallbacks = other.+(this)
    DLCWalletCallbacks(newCallbacks.onStateChange,
                       newCallbacks.onOfferAdd,
                       newCallbacks.onOfferRemove)
  }

  override def start(): Future[Unit] = Future.unit

  private val isStopped: AtomicBoolean = new AtomicBoolean(false)

  /** Completes all streams and waits until they are fully drained */
  override def stop(): Future[Unit] = {
    val start = System.currentTimeMillis()

    //can't complete a stream twice
    if (!isStopped.get()) {
      //complete all queues
      offerAddQueue.complete()
      offerRemoveQueue.complete()
      stateChangeQueue.complete()
      isStopped.set(true)
    } else {
      logger.warn(
        s"Already stopped all queues associated with this DLCWalletCallbackManagerStream")
    }

    for {
      _ <- stateChangeSinkCompleteF
      _ <- offerAddSinkCompleteF
      _ <- offerRemoveSinkCompleteF
    } yield {
      logger.info(
        s"Done draining akka streams for DLCWalletCallbackManagerStream, it took=${System
          .currentTimeMillis() - start}ms")
      ()
    }
  }

  private def matSourceAndQueue[T](
      source: Source[T, SourceQueueWithComplete[T]],
      sink: Sink[T, Future[Done]]): (
      SourceQueueWithComplete[T],
      Future[Done]) = {
    source
      .toMat(sink)(Keep.both)
      .run()
  }
}
