package org.bitcoins.rpc.callback

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
import org.bitcoins.core.api.callback.{OnBlockReceived, OnTxReceived}
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.StartStopAsync

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{ExecutionContext, Future}

case class BitcoindCallbackStreamManager(
    callbacks: BitcoindCallbacks,
    overflowStrategy: OverflowStrategy = OverflowStrategy.backpressure,
    maxBufferSize: Int = 16)(implicit system: ActorSystem)
    extends BitcoindCallbacks
    with StartStopAsync[Unit]
    with BitcoinSLogger {

  import system.dispatcher

  override def start(): Future[Unit] = Future.unit

  private val isStopped: AtomicBoolean = new AtomicBoolean(false)
  override def stop(): Future[Unit] = {
    val start = System.currentTimeMillis()
    if (!isStopped.get()) {
      // complete all queues
      blockQueue.complete()
      txQueue.complete()
      isStopped.set(true)
    }

    for {
      _ <- blockSinkCompleteF
      _ <- txSinkCompleteF
    } yield {
      logger.info(
        s"Done draining akka streams for BitcoindCallbackStreamManager, it took=${System
            .currentTimeMillis() - start}ms"
      )
      ()
    }
  }

  private val blockQueueSource
      : Source[Block, SourceQueueWithComplete[Block]] = {
    Source.queue(maxBufferSize, overflowStrategy)
  }

  private val blockSink: Sink[Block, Future[Done]] = {
    Sink.foreachAsync(1) { case block =>
      callbacks.executeOnBlockReceivedCallbacks(block)
    }
  }

  private val (blockQueue, blockSinkCompleteF) =
    matSourceAndQueue(blockQueueSource, blockSink)

  private val txQueueSource
      : Source[Transaction, SourceQueueWithComplete[Transaction]] = {
    Source.queue(maxBufferSize, overflowStrategy)
  }

  private val txSink: Sink[Transaction, Future[Done]] = {
    Sink.foreachAsync(1) { case tx =>
      callbacks.executeOnTxReceivedCallbacks(tx)
    }
  }

  private val (txQueue, txSinkCompleteF) =
    matSourceAndQueue(txQueueSource, txSink)

  private def matSourceAndQueue[T](
      source: Source[T, SourceQueueWithComplete[T]],
      sink: Sink[T, Future[Done]]
  ): (SourceQueueWithComplete[T], Future[Done]) = {
    source
      .toMat(sink)(Keep.both)
      .run()
  }

  override def onBlockReceived: CallbackHandler[Block, OnBlockReceived] =
    callbacks.onBlockReceived

  override def onTxReceived: CallbackHandler[Transaction, OnTxReceived] = {
    callbacks.onTxReceived
  }

  /** Consider calling [[stop()]] before creating a new instance of callbacks
    * this is because the previous stream will keep running and a new stream
    * will be created
    */
  override def +(other: BitcoindCallbacks): BitcoindCallbacks = {
    val newCallbacks = other.+(this)
    BitcoindCallbackStreamManager(newCallbacks)
  }

  override def executeOnTxReceivedCallbacks(
      tx: Transaction
  )(implicit ec: ExecutionContext): Future[Unit] = {
    txQueue
      .offer(tx)
      .map(_ => ())
  }

  override def executeOnBlockReceivedCallbacks(
      block: Block
  )(implicit ec: ExecutionContext): Future[Unit] = {
    blockQueue
      .offer(block)
      .map(_ => ())
  }
}
