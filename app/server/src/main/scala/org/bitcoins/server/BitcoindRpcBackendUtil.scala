package org.bitcoins.server

import akka.{Done, NotUsed}
import akka.actor.{ActorSystem, Cancellable}
import akka.stream.BoundedSourceQueue
import akka.stream.scaladsl.{Flow, Keep, RunnableGraph, Sink, Source}
import grizzled.slf4j.Logging
import org.bitcoins.chain.ChainCallbacks
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.{
  NeutrinoHDWalletApi,
  NeutrinoWalletApi,
  WalletApi
}
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.V19BlockFilterRpc
import org.bitcoins.rpc.config.ZmqConfig
import org.bitcoins.wallet.Wallet
import org.bitcoins.zmq.ZMQSubscriber

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, Promise}

/** Useful utilities to use in the wallet project for syncing things against bitcoind */
object BitcoindRpcBackendUtil extends Logging {

  /** Has the wallet process all the blocks it has not seen up until bitcoind's chain tip */
  def syncWalletToBitcoind(
      bitcoind: BitcoindRpcClient,
      wallet: NeutrinoHDWalletApi,
      chainCallbacksOpt: Option[ChainCallbacks])(implicit
      system: ActorSystem): Future[Unit] = {
    logger.info("Syncing wallet to bitcoind")
    import system.dispatcher

    val streamF: Future[RunnableGraph[Future[NeutrinoHDWalletApi]]] = for {
      _ <- setSyncingFlag(true, bitcoind, chainCallbacksOpt)
      bitcoindHeight <- bitcoind.getBlockCount
      walletStateOpt <- wallet.getSyncDescriptorOpt()

      heightRange <- {
        walletStateOpt match {
          case None =>
            getHeightRangeNoWalletState(wallet, bitcoind, bitcoindHeight)
          case Some(walletState) =>
            val range = walletState.height.to(bitcoindHeight).tail
            Future.successful(range)
        }
      }
      _ = logger.info(
        s"Syncing from bitcoind with bitcoindHeight=$bitcoindHeight walletHeight=${heightRange.start}")
      syncFlow <- buildBitcoindSyncSink(bitcoind, wallet)
      stream = Source(heightRange).toMat(syncFlow)(Keep.right)
    } yield stream

    //run the stream
    val res = streamF.flatMap(_.run())
    res.onComplete { case _ =>
      setSyncingFlag(false, bitcoind, chainCallbacksOpt)
    }

    res.map(_ => ())
  }

  /** Gets the height range for syncing against bitcoind when we don't have a [[org.bitcoins.core.api.wallet.WalletStateDescriptor]]
    * to read the sync height from.
    */
  private def getHeightRangeNoWalletState(
      wallet: NeutrinoHDWalletApi,
      bitcoind: BitcoindRpcClient,
      bitcoindHeight: Int)(implicit
      ex: ExecutionContext): Future[Range.Inclusive] = {
    for {
      txDbs <- wallet.listTransactions()
      lastConfirmedOpt = txDbs
        .filter(_.blockHashOpt.isDefined)
        .lastOption
      range <- lastConfirmedOpt match {
        case None =>
          val range = (bitcoindHeight - 1).to(bitcoindHeight)
          Future.successful(range)
        case Some(txDb) =>
          for {
            heightOpt <- bitcoind.getBlockHeight(txDb.blockHashOpt.get)
            range <- heightOpt match {
              case Some(height) =>
                logger.info(
                  s"Last tx occurred at block $height, syncing from there")
                val range = height.to(bitcoindHeight)
                Future.successful(range)
              case None =>
                val range = (bitcoindHeight - 1).to(bitcoindHeight)
                Future.successful(range)
            }
          } yield range
      }
    } yield range
  }

  private def setSyncingFlag(
      syncing: Boolean,
      bitcoind: BitcoindRpcClient,
      chainCallbacksOpt: Option[ChainCallbacks])(implicit
      ec: ExecutionContext): Future[Unit] = for {
    _ <- bitcoind.setSyncing(false)
  } yield {
    chainCallbacksOpt.map(_.executeOnSyncFlagChanged(logger, syncing))
    ()
  }

  /** Helper method to sync the wallet until the bitcoind height.
    * This method returns a Sink that you can give block heights too and
    * the sink will synchronize our bitcoin-s wallet against bitcoind
    */
  private def buildBitcoindSyncSink(
      bitcoind: BitcoindRpcClient,
      wallet: NeutrinoHDWalletApi)(implicit
      system: ActorSystem): Future[Sink[Int, Future[NeutrinoHDWalletApi]]] = {
    import system.dispatcher

    val hasFiltersF = bitcoind
      .getBlockHash(0)
      .flatMap(hash => bitcoind.getFilter(hash))
      .map(_ => true)
      .recover { case _: Throwable => false }

    val numParallelism = FutureUtil.getParallelism
    //feeding blockchain hashes into this sync
    //will sync our wallet with those blockchain hashes
    val syncWalletSinkF: Future[
      Sink[DoubleSha256Digest, Future[NeutrinoHDWalletApi]]] = {

      for {
        hasFilters <- hasFiltersF
      } yield {
        if (hasFilters) {
          filterSyncSink(bitcoind.asInstanceOf[V19BlockFilterRpc], wallet)
        } else {
          Flow[DoubleSha256Digest]
            .batch(100, hash => Vector(hash))(_ :+ _)
            .mapAsync(1)(wallet.nodeApi.downloadBlocks(_).map(_ => wallet))
            .toMat(Sink.last)(Keep.right)
        }
      }

    }
    val fetchBlockHashesFlow: Flow[Int, DoubleSha256Digest, NotUsed] = Flow[Int]
      .mapAsync[DoubleSha256Digest](numParallelism) { case height =>
        bitcoind
          .getBlockHash(height)
          .map(_.flip)
      }
    for {
      syncWalletSink <- syncWalletSinkF
    } yield fetchBlockHashesFlow.toMat(syncWalletSink)(Keep.right)

  }

  def createWalletWithBitcoindCallbacks(
      bitcoind: BitcoindRpcClient,
      wallet: Wallet,
      chainCallbacksOpt: Option[ChainCallbacks])(implicit
      system: ActorSystem): Wallet = {
    // We need to create a promise so we can inject the wallet with the callback
    // after we have created it into SyncUtil.getNodeApiWalletCallback
    // so we don't lose the internal state of the wallet
    val walletCallbackP = Promise[Wallet]()

    val nodeApi = BitcoindRpcBackendUtil.buildBitcoindNodeApi(
      bitcoind,
      walletCallbackP.future,
      chainCallbacksOpt)
    val pairedWallet = Wallet(
      nodeApi = nodeApi,
      chainQueryApi = bitcoind,
      feeRateApi = wallet.feeRateApi
    )(wallet.walletConfig)

    walletCallbackP.success(pairedWallet)

    pairedWallet
  }

  def startZMQWalletCallbacks(
      wallet: WalletApi with NeutrinoWalletApi,
      zmqConfig: ZmqConfig): Unit = {
    require(zmqConfig != ZmqConfig.empty,
            "Must have the zmq raw configs defined to setup ZMQ callbacks")

    zmqConfig.rawTx.foreach { zmq =>
      val rawTxListener: Option[Transaction => Unit] = Some {
        { tx: Transaction =>
          logger.debug(s"Received tx ${tx.txIdBE.hex}, processing")
          wallet.processTransaction(tx, None)
          ()
        }
      }

      new ZMQSubscriber(socket = zmq,
                        hashTxListener = None,
                        hashBlockListener = None,
                        rawTxListener = rawTxListener,
                        rawBlockListener = None).start()
    }

    zmqConfig.rawBlock.foreach { zmq =>
      val rawBlockListener: Option[Block => Unit] = Some {
        { block: Block =>
          logger.debug(
            s"Received block ${block.blockHeader.hashBE.hex}, processing")
          wallet.processBlock(block)
          ()
        }
      }

      new ZMQSubscriber(socket = zmq,
                        hashTxListener = None,
                        hashBlockListener = None,
                        rawTxListener = None,
                        rawBlockListener = rawBlockListener).start()
    }
  }

  def createDLCWalletWithBitcoindCallbacks(
      bitcoind: BitcoindRpcClient,
      wallet: DLCWallet,
      chainCallbacksOpt: Option[ChainCallbacks])(implicit
      system: ActorSystem): DLCWallet = {
    // We need to create a promise so we can inject the wallet with the callback
    // after we have created it into SyncUtil.getNodeApiWalletCallback
    // so we don't lose the internal state of the wallet
    val walletCallbackP = Promise[Wallet]()

    val pairedWallet = DLCWallet(
      nodeApi =
        BitcoindRpcBackendUtil.buildBitcoindNodeApi(bitcoind,
                                                    walletCallbackP.future,
                                                    chainCallbacksOpt),
      chainQueryApi = bitcoind,
      feeRateApi = wallet.feeRateApi
    )(wallet.walletConfig, wallet.dlcConfig)

    walletCallbackP.success(pairedWallet)

    pairedWallet
  }

  private def filterSyncSink(
      bitcoindRpcClient: V19BlockFilterRpc,
      wallet: NeutrinoHDWalletApi)(implicit system: ActorSystem): Sink[
    DoubleSha256Digest,
    Future[NeutrinoHDWalletApi]] = {
    import system.dispatcher

    val numParallelism = FutureUtil.getParallelism
    val sink: Sink[DoubleSha256Digest, Future[NeutrinoHDWalletApi]] =
      Flow[DoubleSha256Digest]
        .mapAsync(parallelism = numParallelism) { hash =>
          bitcoindRpcClient.getBlockFilter(hash.flip, FilterType.Basic).map {
            res => (hash, res.filter)
          }
        }
        .batch(1000, filter => Vector(filter))(_ :+ _)
        .foldAsync(wallet) { case (wallet, filterRes) =>
          wallet.processCompactFilters(filterRes)
        }
        .toMat(Sink.last)(Keep.right)

    sink
  }

  /** Creates an anonymous [[NodeApi]] that downloads blocks using
    * akka streams from bitcoind, and then calls [[Wallet.processBlock]]
    */
  def buildBitcoindNodeApi(
      bitcoindRpcClient: BitcoindRpcClient,
      walletF: Future[WalletApi],
      chainCallbacksOpt: Option[ChainCallbacks])(implicit
      system: ActorSystem): NodeApi = {
    import system.dispatcher
    new NodeApi {

      override def downloadBlocks(
          blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = {
        logger.info(s"Fetching ${blockHashes.length} blocks from bitcoind")
        val numParallelism = FutureUtil.getParallelism
        walletF
          .flatMap { wallet =>
            val runStream: Future[Done] = Source(blockHashes)
              .mapAsync(parallelism = numParallelism) { hash =>
                val blockF = bitcoindRpcClient.getBlockRaw(hash)
                val blockHeaderResultF = bitcoindRpcClient.getBlockHeader(hash)
                for {
                  block <- blockF
                  blockHeaderResult <- blockHeaderResultF
                } yield (block, blockHeaderResult)
              }
              .foldAsync(wallet) { case (wallet, (block, blockHeaderResult)) =>
                val blockProcessedF = wallet.processBlock(block)
                val executeCallbackF: Future[WalletApi] =
                  blockProcessedF.flatMap { wallet =>
                    chainCallbacksOpt match {
                      case None           => Future.successful(wallet)
                      case Some(callback) =>
                        //this can be slow as we aren't batching headers at all
                        val headerWithHeights =
                          Vector((blockHeaderResult.height, block.blockHeader))
                        val f = callback
                          .executeOnBlockHeaderConnectedCallbacks(
                            logger,
                            headerWithHeights)
                        f.map(_ => wallet)
                    }
                  }
                executeCallbackF
              }
              .run()
            runStream.map(_ => wallet)
          }
          .flatMap(_.updateUtxoPendingStates())
          .map(_ => ())
      }

      /** Broadcasts the given transaction over the P2P network
        */
      override def broadcastTransactions(
          transactions: Vector[Transaction]): Future[Unit] = {
        bitcoindRpcClient.broadcastTransactions(transactions)
      }
    }
  }

  /** Starts the [[ActorSystem]] to poll the [[BitcoindRpcClient]] for its block count,
    * if it has changed, it will then request those blocks to process them
    *
    * @param startCount The starting block height of the wallet
    * @param interval   The amount of time between polls, this should not be too aggressive
    *                   as the wallet will need to process the new blocks
    */
  def startBitcoindBlockPolling(
      wallet: WalletApi,
      bitcoind: BitcoindRpcClient,
      chainCallbacksOpt: Option[ChainCallbacks],
      interval: FiniteDuration = 10.seconds)(implicit
      system: ActorSystem): Cancellable = {
    import system.dispatcher

    val processingBitcoindBlocks = new AtomicBoolean(false)

    system.scheduler.scheduleWithFixedDelay(0.seconds, interval) { () =>
      {
        if (processingBitcoindBlocks.compareAndSet(false, true)) {
          val f = for {
            walletSyncState <- wallet.getSyncState()
            rescanning <- wallet.isRescanning()
            res <-
              if (!rescanning) {
                val pollFOptF = pollBitcoind(wallet = wallet,
                                             bitcoind = bitcoind,
                                             chainCallbacksOpt =
                                               chainCallbacksOpt,
                                             prevCount = walletSyncState.height)

                pollFOptF.flatMap {
                  case Some(pollF) => pollF
                  case None        => Future.unit
                }
              } else {
                logger.info(
                  s"Skipping scanning the blockchain during wallet rescan")
                Future.unit
              }
          } yield res

          f.onComplete { _ =>
            processingBitcoindBlocks.set(false)
            BitcoindRpcBackendUtil.setSyncingFlag(false,
                                                  bitcoind,
                                                  chainCallbacksOpt)
          } //reset polling variable
          f.failed.foreach(err => logger.error(s"Failed to poll bitcoind", err))
        } else {
          logger.info(s"Previous bitcoind polling still running")
        }
      }
    }
  }

  /** Polls bitcoind for syncing the blockchain
    * @return None if there was nothing to sync, else the Future[Done] that is completed when the sync is finished.
    */
  private def pollBitcoind(
      wallet: WalletApi,
      bitcoind: BitcoindRpcClient,
      chainCallbacksOpt: Option[ChainCallbacks],
      prevCount: Int)(implicit
      system: ActorSystem): Future[Option[Future[Done]]] = {
    import system.dispatcher
    val atomicPrevCount = new AtomicInteger(prevCount)
    val queueSource: Source[Int, BoundedSourceQueue[Int]] = Source.queue(100)
    val numParallelism = FutureUtil.getParallelism
    val processBlockSink = Sink.foreachAsync[Vector[DoubleSha256Digest]](1) {
      case hashes: Vector[DoubleSha256Digest] =>
        val requestsBlocksF = wallet.nodeApi.downloadBlocks(hashes)

        requestsBlocksF.failed.foreach { case err =>
          val failedCount = atomicPrevCount.get
          atomicPrevCount.set(prevCount)
          logger.error(
            s"Requesting blocks from bitcoind polling failed, range=[$prevCount, $failedCount]",
            err)
        }

        requestsBlocksF
    }
    val (queue, doneF) = queueSource
      .mapAsync(parallelism = numParallelism) { height: Int =>
        bitcoind.getBlockHash(height).map(_.flip)
      }
      .map { hash =>
        val _ = atomicPrevCount.incrementAndGet()
        hash
      }
      .batch(100, seed = hash => Vector(hash))(_ :+ _)
      .toMat(processBlockSink)(Keep.both)
      .run()
    logger.trace("Polling bitcoind for block count")

    val resF: Future[Unit] = for {
      _ <- bitcoind.setSyncing(true)
      _ <- setSyncingFlag(true, bitcoind, chainCallbacksOpt)
      count <- bitcoind.getBlockCount
      retval <- {
        if (prevCount < count) {
          logger.info(
            s"Bitcoind has new block(s), requesting... ${count - prevCount} blocks")

          // use .tail so we don't process the previous block that we already did
          val range = prevCount.to(count).tail

          range.foreach(r => queue.offer(r))
          queue.complete() //complete the stream after offering al heights we need ot sync
          Future.unit
        } else if (prevCount > count) {
          Future.failed(new RuntimeException(
            s"Bitcoind is at a block height ($count) before the wallet's ($prevCount)"))
        } else {
          logger.debug(s"In sync $prevCount count=$count")
          Future.unit
        }
      }
    } yield {
      retval
    }
    resF.map(_ => Some(doneF))
  }

  def startBitcoindMempoolPolling(
      wallet: WalletApi,
      bitcoind: BitcoindRpcClient,
      interval: FiniteDuration = 10.seconds)(
      processTx: Transaction => Future[Unit])(implicit
      system: ActorSystem,
      ec: ExecutionContext): Cancellable = {
    @volatile var prevMempool: Set[DoubleSha256DigestBE] =
      Set.empty[DoubleSha256DigestBE]

    def getDiffAndReplace(
        newMempool: Set[DoubleSha256DigestBE]): Set[DoubleSha256DigestBE] =
      synchronized {
        val txids = newMempool.diff(prevMempool)
        prevMempool = newMempool
        txids
      }

    val processingMempool = new AtomicBoolean(false)

    def pollMempool(): Future[Unit] = {
      if (processingMempool.compareAndSet(false, true)) {
        logger.debug("Polling bitcoind for mempool")
        val numParallelism = FutureUtil.getParallelism

        //don't want to execute these in parallel
        val processTxFlow = Sink.foreachAsync[Option[Transaction]](1) {
          case Some(tx) => processTx(tx)
          case None     => Future.unit
        }

        val res = for {
          mempool <- bitcoind.getRawMemPool
          newTxIds = getDiffAndReplace(mempool.toSet)
          _ = logger.debug(s"Found ${newTxIds.size} new mempool transactions")

          _ <- Source(newTxIds)
            .mapAsync(parallelism = numParallelism) { txid =>
              bitcoind
                .getRawTransactionRaw(txid)
                .map(Option(_))
                .recover { case _: Throwable =>
                  None
                }
            }
            .toMat(processTxFlow)(Keep.right)
            .run()
        } yield {
          logger.debug(
            s"Done processing ${newTxIds.size} new mempool transactions")
          ()
        }
        res.onComplete(_ => processingMempool.set(false))
        res
      } else {
        logger.info(
          s"Skipping scanning the mempool since a previously scheduled task is still running")
        Future.unit
      }
    }

    system.scheduler.scheduleWithFixedDelay(0.seconds, interval) { () =>
      {
        val f = for {
          rescanning <- wallet.isRescanning()
          res <-
            if (!rescanning) {
              pollMempool()
            } else {
              logger.info(s"Skipping scanning the mempool during wallet rescan")
              Future.unit
            }
        } yield res

        f.failed.foreach(err => logger.error(s"Failed to poll mempool", err))
        ()
      }
    }
  }

}
