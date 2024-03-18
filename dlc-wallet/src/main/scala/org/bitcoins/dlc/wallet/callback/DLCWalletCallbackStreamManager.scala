package org.bitcoins.dlc.wallet.callback

import org.apache.pekko.Done
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.OverflowStrategy
import org.apache.pekko.stream.scaladsl.{
  Keep,
  Sink,
  Source,
  SourceQueueWithComplete
}
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.CallbackHandler
import org.bitcoins.core.api.dlc.wallet.db.IncomingDLCOfferDb
import org.bitcoins.core.protocol.dlc.models.DLCStatus
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.crypto.Sha256Digest

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.Future

case class DLCWalletCallbackStreamManager(
    callbacks: DLCWalletCallbacks,
    overflowStrategy: OverflowStrategy = OverflowStrategy.backpressure,
    maxBufferSize: Int = 16)(implicit system: ActorSystem)
    extends DLCWalletCallbacks
    with StartStopAsync[Unit]
    with BitcoinSLogger {

  import system.dispatcher

  private val stateChangeSource: Source[
    DLCStatus,
    SourceQueueWithComplete[DLCStatus]] = {
    Source.queue(maxBufferSize, overflowStrategy)
  }

  private val stateChangeSink: Sink[DLCStatus, Future[Done]] = {
    Sink.foreachAsync(1) { case state =>
      callbacks.executeOnDLCStateChange(state)
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
      callbacks.executeOnDLCOfferAdd(offer)
    }
  }

  private val (offerAddQueue, offerAddSinkCompleteF) = {
    matSourceAndQueue(offerAddSource, offerAddSink)
  }

  private val offerRemoveSource: Source[
    Sha256Digest,
    SourceQueueWithComplete[Sha256Digest]] = {
    Source.queue(maxBufferSize, overflowStrategy)
  }

  private val offerRemoveSink: Sink[Sha256Digest, Future[Done]] = {
    Sink.foreachAsync(1) { case offer =>
      callbacks.executeOnDLCOfferRemove(offer)
    }
  }

  private val (offerRemoveQueue, offerRemoveSinkCompleteF) = {
    matSourceAndQueue(offerRemoveSource, offerRemoveSink)
  }

  override def onStateChange: CallbackHandler[DLCStatus, OnDLCStateChange] = {
    callbacks.onStateChange
  }

  override def onOfferAdd: CallbackHandler[
    IncomingDLCOfferDb,
    OnDLCOfferAdd] = {
    callbacks.onOfferAdd
  }

  override def onOfferRemove: CallbackHandler[
    Sha256Digest,
    OnDLCOfferRemove] = {
    callbacks.onOfferRemove
  }

  override def +(other: DLCWalletCallbacks): DLCWalletCallbacks = {
    val newCallbacks = other.+(this)
    DLCWalletCallbackStreamManager(newCallbacks)
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
