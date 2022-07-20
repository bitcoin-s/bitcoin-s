package org.bitcoins.node.callback

import akka.Done
import akka.actor.ActorSystem
import akka.stream.BoundedSourceQueue
import akka.stream.scaladsl.{Keep, Sink, Source}
import grizzled.slf4j.{Logger, Logging}
import monix.execution.atomic.AtomicBoolean
import org.bitcoins.core.api.CallbackHandler
import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, MerkleBlock}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.node.{
  NodeCallbacks,
  OnBlockHeadersReceived,
  OnBlockReceived,
  OnCompactFiltersReceived,
  OnMerkleBlockReceived,
  OnTxReceived
}

import scala.concurrent.{ExecutionContext, Future}

/** Creates a wrapper around the give node callbacks with a stream */
case class NodeCallbackStreamManager(callbacks: NodeCallbacks)(implicit
    system: ActorSystem)
    extends NodeCallbacks
    with StartStopAsync[Unit]
    with Logging {
  import system.dispatcher
  private val maxBufferSize: Int = 25

  private val filterQueueSource: Source[
    Vector[(DoubleSha256Digest, GolombFilter)],
    BoundedSourceQueue[Vector[(DoubleSha256Digest, GolombFilter)]]] = {
    Source.queue(maxBufferSize)
  }

  private val filterSink: Sink[
    Vector[(DoubleSha256Digest, GolombFilter)],
    Future[Done]] = {
    Sink.foreachAsync(1) { case vec =>
      callbacks.executeOnCompactFiltersReceivedCallbacks(logger, vec)
    }
  }

  private val (filterQueue, filterSinkCompleteF) =
    matSourceAndQueue(filterQueueSource, filterSink)

  private val txQueueSource: Source[
    Transaction,
    BoundedSourceQueue[Transaction]] = {
    Source.queue(maxBufferSize)
  }

  private val txSink: Sink[Transaction, Future[Done]] = {
    Sink.foreachAsync(1) { case tx =>
      callbacks.executeOnTxReceivedCallbacks(logger, tx)
    }
  }

  private val (txQueue, txSinkCompleteF) =
    matSourceAndQueue(txQueueSource, txSink)

  private val headerQueueSource: Source[
    Vector[BlockHeader],
    BoundedSourceQueue[Vector[BlockHeader]]] = {
    Source.queue(maxBufferSize)
  }

  private val headerSink: Sink[Vector[BlockHeader], Future[Done]] = {
    Sink.foreachAsync(1) { case headers =>
      callbacks.executeOnBlockHeadersReceivedCallbacks(logger, headers)
    }
  }

  private val (headerQueue, headerSinkCompleteF) =
    matSourceAndQueue(headerQueueSource, headerSink)

  private val blockQueueSource: Source[Block, BoundedSourceQueue[Block]] = {
    Source.queue(maxBufferSize)
  }

  private val blockSink: Sink[Block, Future[Done]] = {
    Sink.foreachAsync(1) { case block =>
      callbacks.executeOnBlockReceivedCallbacks(logger, block)
    }
  }

  private val (blockQueue, blockSinkCompleteF) =
    matSourceAndQueue(blockQueueSource, blockSink)

  private val merkleBlockQueueSource: Source[
    (MerkleBlock, Vector[Transaction]),
    BoundedSourceQueue[(MerkleBlock, Vector[Transaction])]] = {
    Source.queue(maxBufferSize)
  }

  private val merkleBlockSink: Sink[
    (MerkleBlock, Vector[Transaction]),
    Future[Done]] = {
    Sink.foreachAsync(1) { case tuple =>
      callbacks.executeOnMerkleBlockReceivedCallbacks(logger = logger,
                                                      merkleBlock = tuple._1,
                                                      txs = tuple._2)
    }
  }

  private val (merkleBlockQueue, merkleBlockCompleteF) =
    matSourceAndQueue(merkleBlockQueueSource, merkleBlockSink)

  override def start(): Future[Unit] = Future.unit

  private val isStopped: AtomicBoolean = AtomicBoolean(false)

  /** Completes all streams and waits until they are fully drained */
  override def stop(): Future[Unit] = {
    val start = System.currentTimeMillis()

    //can't complete a stream twice
    if (!isStopped.get()) {
      //complete all queues
      filterQueue.complete()
      txQueue.complete()
      headerQueue.complete()
      merkleBlockQueue.complete()
      blockQueue.complete()
      isStopped.set(true)
    } else {
      logger.warn(
        s"Already stopped all queues associated with this NodeCallBackStreamManager")
    }

    for {
      _ <- filterSinkCompleteF
      _ <- txSinkCompleteF
      _ <- headerSinkCompleteF
      _ <- merkleBlockCompleteF
      _ <- blockSinkCompleteF
    } yield {
      logger.info(
        s"Done draining akka streams for NodeCallbackStreamManager, it took=${System
          .currentTimeMillis() - start}ms")
      ()
    }
  }

  private def matSourceAndQueue[T](
      source: Source[T, BoundedSourceQueue[T]],
      sink: Sink[T, Future[Done]]): (BoundedSourceQueue[T], Future[Done]) = {
    source
      .toMat(sink)(Keep.both)
      .run()
  }

  override def onCompactFiltersReceived: CallbackHandler[
    Vector[(DoubleSha256Digest, GolombFilter)],
    OnCompactFiltersReceived] = {
    callbacks.onCompactFiltersReceived
  }

  override def onTxReceived: CallbackHandler[Transaction, OnTxReceived] = {
    callbacks.onTxReceived
  }

  override def onBlockReceived: CallbackHandler[Block, OnBlockReceived] =
    callbacks.onBlockReceived

  override def onMerkleBlockReceived: CallbackHandler[
    (MerkleBlock, Vector[Transaction]),
    OnMerkleBlockReceived] = callbacks.onMerkleBlockReceived

  override def onBlockHeadersReceived: CallbackHandler[
    Vector[BlockHeader],
    OnBlockHeadersReceived] = callbacks.onBlockHeadersReceived

  override def executeOnTxReceivedCallbacks(logger: Logger, tx: Transaction)(
      implicit ec: ExecutionContext): Future[Unit] = {
    txQueue.offer(tx)
    Future.unit
  }

  override def executeOnBlockReceivedCallbacks(logger: Logger, block: Block)(
      implicit ec: ExecutionContext): Future[Unit] = {
    blockQueue.offer(block)
    Future.unit
  }

  override def executeOnCompactFiltersReceivedCallbacks(
      logger: Logger,
      blockFilters: Vector[(DoubleSha256Digest, GolombFilter)])(implicit
      ec: ExecutionContext): Future[Unit] = {
    filterQueue.offer(blockFilters)
    Future.unit
  }

  override def executeOnMerkleBlockReceivedCallbacks(
      logger: Logger,
      merkleBlock: MerkleBlock,
      txs: Vector[Transaction])(implicit ec: ExecutionContext): Future[Unit] = {
    merkleBlockQueue.offer((merkleBlock, txs))
    Future.unit
  }

  override def executeOnBlockHeadersReceivedCallbacks(
      logger: Logger,
      headers: Vector[BlockHeader])(implicit
      ec: ExecutionContext): Future[Unit] = {
    headerQueue.offer(headers)
    Future.unit
  }

  /** Consider calling [[stop()]] before creating a new instance of callbacks
    * this is because the previous stream will keep running and a new stream
    * will be created
    */
  override def +(other: NodeCallbacks): NodeCallbacks = {
    val newCallbacks = other.+(this)
    NodeCallbackStreamManager(newCallbacks)
  }
}
