package org.bitcoins.server

import org.apache.pekko.actor.{ActorSystem, Cancellable}
import org.apache.pekko.stream.OverflowStrategy
import org.apache.pekko.stream.scaladsl.{
  Flow,
  Keep,
  RunnableGraph,
  Sink,
  Source,
  SourceQueueWithComplete
}
import org.apache.pekko.{Done, NotUsed}
import org.bitcoins.chain.ChainCallbacks
import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockHeaderResult
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.{NeutrinoHDWalletApi, WalletApi}
import org.bitcoins.core.config.{MainNet, RegTest, SigNet, TestNet3}
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BlockchainRpc}
import org.bitcoins.rpc.config.ZmqConfig
import org.bitcoins.rpc.util.BitcoindStreamUtil
import org.bitcoins.wallet.Wallet
import org.bitcoins.zmq.ZMQSubscriber

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}

/** Useful utilities to use in the wallet project for syncing things against
  * bitcoind
  */
object BitcoindRpcBackendUtil extends BitcoinSLogger {

  /** Has the wallet process all the blocks it has not seen up until bitcoind's
    * chain tip
    */
  def syncWalletToBitcoind(
      bitcoind: BitcoindRpcClient,
      wallet: NeutrinoHDWalletApi,
      chainCallbacksOpt: Option[ChainCallbacks]
  )(implicit system: ActorSystem): Future[Unit] = {
    logger.info("Syncing wallet to bitcoind")
    import system.dispatcher

    val streamF: Future[RunnableGraph[Future[NeutrinoHDWalletApi]]] = for {
      _ <- setSyncingFlag(syncing = true, bitcoind, chainCallbacksOpt)
      bitcoindHeight <- bitcoind.getBlockCount()
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
        s"Syncing from bitcoind with bitcoindHeight=$bitcoindHeight walletHeight=${heightRange.start}"
      )
      syncFlow <- buildBitcoindSyncSink(bitcoind, wallet)
      stream = Source(heightRange).toMat(syncFlow)(Keep.right)
    } yield stream

    // run the stream
    val res = streamF.flatMap(_.run())
    res.onComplete { case _ =>
      val isBitcoindInSyncF = BitcoindRpcBackendUtil.isBitcoindInSync(bitcoind)
      isBitcoindInSyncF.flatMap { isBitcoindInSync =>
        if (isBitcoindInSync) {
          // if bitcoind is in sync, and we are in sync with bitcoind, set the syncing flag to false
          setSyncingFlag(false, bitcoind, chainCallbacksOpt)
        } else {
          // if bitcoind is not in sync, we cannot be done syncing. Keep the syncing flag to true
          // so do nothing in this case
          logger.warn(
            s"We synced against bitcoind, but bitcoind is not in sync with the network."
          )
          Future.unit
        }
      }
    }

    res.map(_ => ())
  }

  /** Gets the height range for syncing against bitcoind when we don't have a
    * [[org.bitcoins.core.api.wallet.WalletStateDescriptor]] to read the sync
    * height from.
    */
  private def getHeightRangeNoWalletState(
      wallet: NeutrinoHDWalletApi,
      bitcoind: BitcoindRpcClient,
      bitcoindHeight: Int
  )(implicit ex: ExecutionContext): Future[Range.Inclusive] = {
    for {
      txDbs <- wallet.transactionProcessing.listTransactions()
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
                  s"Last tx occurred at block $height, syncing from there"
                )
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
      chainCallbacksOpt: Option[ChainCallbacks]
  )(implicit ec: ExecutionContext): Future[Unit] = {
    val oldSyncingFlagF = bitcoind.isSyncing()
    for {
      oldFlag <- oldSyncingFlagF
      _ <- bitcoind.setSyncing(syncing)
      _ <- {
        if (oldFlag != syncing) {
          val executeCallbackOpt =
            chainCallbacksOpt.map(_.executeOnSyncFlagChanged(syncing))
          executeCallbackOpt match {
            case Some(f) => f
            case None    => Future.unit
          }
        } else {
          Future.unit
        }
      }
    } yield {
      ()
    }
  }

  /** Helper method to sync the wallet until the bitcoind height. This method
    * returns a Sink that you can give block heights too and the sink will
    * synchronize our bitcoin-s wallet against bitcoind
    */
  private def buildBitcoindSyncSink(
      bitcoind: BitcoindRpcClient,
      wallet: NeutrinoHDWalletApi
  )(implicit
      system: ActorSystem
  ): Future[Sink[Int, Future[NeutrinoHDWalletApi]]] = {
    import system.dispatcher

    val hasFiltersF = bitcoind
      .getBlockHash(0)
      .flatMap(hash => bitcoind.getFilter(hash))
      .map(_ => true)
      .recover { case _: Throwable => false }

    val numParallelism = FutureUtil.getParallelism
    // feeding blockchain hashes into this sync
    // will sync our wallet with those blockchain hashes
    val syncWalletSinkF
        : Future[Sink[DoubleSha256DigestBE, Future[NeutrinoHDWalletApi]]] = {

      for {
        hasFilters <- hasFiltersF
      } yield {
        if (hasFilters) {
          filterSyncSink(bitcoind, wallet)
        } else {
          Flow[DoubleSha256DigestBE]
            .batch(100, hash => Vector(hash))(_ :+ _)
            .mapAsync(1)(wallet.nodeApi.downloadBlocks(_).map(_ => wallet))
            .toMat(Sink.last)(Keep.right)
        }
      }

    }
    val fetchBlockHashesFlow: Flow[Int, DoubleSha256DigestBE, NotUsed] =
      Flow[Int]
        .mapAsync(numParallelism) { case height =>
          bitcoind
            .getBlockHash(height)
        }
    for {
      syncWalletSink <- syncWalletSinkF
    } yield fetchBlockHashesFlow.toMat(syncWalletSink)(Keep.right)

  }

  def createWalletWithBitcoindCallbacks(
      bitcoind: BitcoindRpcClient,
      wallet: Wallet,
      chainCallbacksOpt: Option[ChainCallbacks]
  )(implicit system: ActorSystem): Wallet = {
    val nodeApi = BitcoindRpcBackendUtil.buildBitcoindNodeApi(
      bitcoind,
      wallet,
      chainCallbacksOpt
    )
    val pairedWallet = Wallet(
      nodeApi = nodeApi,
      chainQueryApi = bitcoind
    )(wallet.walletConfig)

    pairedWallet
  }

  def startZMQWalletCallbacks(
      wallet: NeutrinoHDWalletApi,
      zmqConfig: ZmqConfig
  )(implicit ec: ExecutionContext): WalletZmqSubscribers = {
    require(
      zmqConfig != ZmqConfig.empty,
      "Must have the zmq raw configs defined to setup ZMQ callbacks"
    )

    val rawTxSub = zmqConfig.rawTx.map { zmq =>
      val rawTxListener: Option[Transaction => Unit] = Some {
        { (tx: Transaction) =>
          logger.debug(s"Received tx ${tx.txIdBE.hex}, processing")
          val f = wallet.transactionProcessing.processTransaction(tx, None)
          f.failed.foreach { err =>
            logger.error("failed to process raw tx zmq message", err)
          }
          ()
        }
      }

      new ZMQSubscriber(
        socket = zmq,
        hashTxListener = None,
        hashBlockListener = None,
        rawTxListener = rawTxListener,
        rawBlockListener = None
      )
    }

    val rawBlockSub = zmqConfig.rawBlock.map { zmq =>
      val rawBlockListener: Option[Block => Unit] = Some {
        { (block: Block) =>
          logger.info(
            s"Received block ${block.blockHeader.hashBE.hex}, processing"
          )
          val f = wallet.transactionProcessing.processBlock(block)
          f.failed.foreach { err =>
            logger.error("failed to process raw block zmq message", err)
          }
          ()
        }
      }

      new ZMQSubscriber(
        socket = zmq,
        hashTxListener = None,
        hashBlockListener = None,
        rawTxListener = None,
        rawBlockListener = rawBlockListener
      )
    }

    val subs = WalletZmqSubscribers(rawTxSub, rawBlockSub)
    subs.start()
    subs
  }

  def createDLCWalletWithBitcoindCallbacks(
      bitcoind: BitcoindRpcClient,
      wallet: DLCWallet,
      chainCallbacksOpt: Option[ChainCallbacks]
  )(implicit system: ActorSystem): DLCWallet = {
    val nodeApi = BitcoindRpcBackendUtil.buildBitcoindNodeApi(
      bitcoind,
      wallet,
      chainCallbacksOpt
    )

    val walletConfig = wallet.walletConfig
    val dlcConfig = wallet.dlcConfig
    val bitcoindCallbackWallet =
      wallet.walletApi.copy(nodeApi = nodeApi)(walletConfig)
    val pairedWallet =
      DLCWallet(bitcoindCallbackWallet)(dlcConfig, walletConfig)

    pairedWallet
  }

  private def filterSyncSink(
      bitcoindRpcClient: BlockchainRpc,
      wallet: NeutrinoHDWalletApi
  )(implicit
      ec: ExecutionContext
  ): Sink[DoubleSha256DigestBE, Future[NeutrinoHDWalletApi]] = {

    val numParallelism = FutureUtil.getParallelism
    val sink: Sink[DoubleSha256DigestBE, Future[NeutrinoHDWalletApi]] =
      Flow[DoubleSha256DigestBE]
        .mapAsync(parallelism = numParallelism) { hash =>
          bitcoindRpcClient.getBlockFilter(hash, FilterType.Basic).map { res =>
            (hash, res.filter)
          }
        }
        .batch(1000, filter => Vector(filter))(_ :+ _)
        .foldAsync(wallet) { case (wallet, filterRes) =>
          wallet.processCompactFilters(filterRes)
        }
        .toMat(Sink.last)(Keep.right)

    sink
  }

  /** Creates an anonymous [[NodeApi]] that downloads blocks using akka streams
    * from bitcoind, and then calls [[NeutrinoWalletApi.processBlock]]
    */
  def buildBitcoindNodeApi(
      bitcoindRpcClient: BitcoindRpcClient,
      wallet: WalletApi,
      chainCallbacksOpt: Option[ChainCallbacks]
  )(implicit system: ActorSystem): NodeApi = {
    import system.dispatcher
    new NodeApi {

      override def downloadBlocks(
          blockHashes: Vector[DoubleSha256DigestBE]
      ): Future[Unit] = {
        logger.info(s"Fetching ${blockHashes.length} blocks from bitcoind")
        val numParallelism = FutureUtil.getParallelism
        val source = Source(blockHashes)
        val fetchBlocksFlow = BitcoindStreamUtil.fetchBlocksBitcoind(
          bitcoindRpcClient = bitcoindRpcClient,
          parallelism = numParallelism
        )

        val sink: Sink[(Block, GetBlockHeaderResult), Future[Done]] = {
          Sink.foreachAsync(1) {
            case (block: Block, blockHeaderResult: GetBlockHeaderResult) =>
              val blockProcessedF =
                wallet.transactionProcessing.processBlock(block)
              val executeCallbackF: Future[Unit] = {
                for {
                  wallet <- blockProcessedF
                  _ <- handleChainCallbacks(
                    chainCallbacksOpt,
                    blockHeaderResult
                  )
                } yield wallet
              }

              executeCallbackF
          }
        }

        val doneF: Future[Done] =
          source
            .via(fetchBlocksFlow)
            .toMat(sink)(Keep.right)
            .run()

        for {
          _ <- doneF
          _ <- wallet.utxoHandling.updateUtxoPendingStates()
        } yield ()
      }

      /** Broadcasts the given transaction over the P2P network
        */
      override def broadcastTransactions(
          transactions: Vector[Transaction]
      ): Future[Unit] = {
        bitcoindRpcClient.broadcastTransactions(transactions)
      }

      override def getConnectionCount: Future[Int] = {
        bitcoindRpcClient.getConnectionCount
      }
    }
  }

  private def handleChainCallbacks(
      chainCallbacksOpt: Option[ChainCallbacks],
      blockHeaderResult: GetBlockHeaderResult
  )(implicit ec: ExecutionContext): Future[Unit] = {
    chainCallbacksOpt match {
      case None           => Future.unit
      case Some(callback) =>
        // this can be slow as we aren't batching headers at all
        val headerWithHeights =
          Vector((blockHeaderResult.height, blockHeaderResult.blockHeader))
        val f = callback
          .executeOnBlockHeaderConnectedCallbacks(headerWithHeights)
        f
    }
  }

  /** Starts the [[ActorSystem]] to poll the [[BitcoindRpcClient]] for its block
    * count, if it has changed, it will then request those blocks to process
    * them
    *
    * @param startCount
    *   The starting block height of the wallet
    * @param interval
    *   The amount of time between polls, this should not be too aggressive as
    *   the wallet will need to process the new blocks. If you do not provide
    *   this we will intelligently choose one depending on the network we are on
    */
  def startBitcoindBlockPolling(
      wallet: WalletApi,
      bitcoind: BitcoindRpcClient,
      chainCallbacksOpt: Option[ChainCallbacks],
      intervalOpt: Option[FiniteDuration] = None
  )(processBlock: Block => Future[Unit])(implicit
      system: ActorSystem): Cancellable = {
    import system.dispatcher
    val interval = intervalOpt.getOrElse {
      bitcoind.bitcoindRpcAppConfig.network match {
        case MainNet | TestNet3 | SigNet => 10.seconds
        case RegTest                     => 1.second
      }
    }
    val processingBitcoindBlocks = new AtomicBoolean(false)

    system.scheduler.scheduleWithFixedDelay(0.seconds, interval) { () =>
      {
        val isBitcoindSyncedF = isBitcoindInSync(bitcoind)
        isBitcoindSyncedF.map { isBitcoindSynced =>
          if (!isBitcoindSynced) {
            logger.info(s"Bitcoind is not synced, waiting for IBD to complete.")
          } else if (processingBitcoindBlocks.compareAndSet(false, true)) {
            val f = for {
              walletSyncState <- wallet.getSyncState()
              rescanning <- wallet.isRescanning()
              res <-
                if (!rescanning) {
                  val pollFOptF =
                    pollBitcoind(
                      bitcoind = bitcoind,
                      chainCallbacksOpt = chainCallbacksOpt,
                      prevCount = walletSyncState.height,
                      processBlock = processBlock
                    )

                  pollFOptF.flatMap {
                    case Some(pollF) => pollF
                    case None        => Future.unit
                  }
                } else {
                  logger.info(
                    s"Skipping scanning the blockchain during wallet rescan"
                  )
                  Future.unit
                }
            } yield res

            f.onComplete { _ =>
              processingBitcoindBlocks.set(false)
              BitcoindRpcBackendUtil.setSyncingFlag(
                syncing = false,
                bitcoind = bitcoind,
                chainCallbacksOpt = chainCallbacksOpt
              )
            } // reset polling variable
            f.failed.foreach(err =>
              logger.error(s"Failed to poll bitcoind", err))
          } else {
            logger.info(s"Previous bitcoind polling still running")
          }
        }
        ()
      }
    }
  }

  /** Polls bitcoind for syncing the blockchain
    * @return
    *   None if there was nothing to sync, else the Future[Done] that is
    *   completed when the sync is finished.
    */
  private def pollBitcoind(
      bitcoind: BitcoindRpcClient,
      chainCallbacksOpt: Option[ChainCallbacks],
      prevCount: Int,
      processBlock: Block => Future[Unit]
  )(implicit system: ActorSystem): Future[Option[Future[Done]]] = {
    import system.dispatcher
    val atomicPrevCount = new AtomicInteger(prevCount)
    val queueSource: Source[Int, SourceQueueWithComplete[Int]] =
      Source.queue[Int](16, OverflowStrategy.backpressure)
    val numParallelism = FutureUtil.getParallelism

    val fetchBlocksFlow =
      BitcoindStreamUtil.fetchBlocksBitcoind(bitcoind, numParallelism)

    val processBlockSink: Sink[(Block, GetBlockHeaderResult), Future[Done]] = {
      Sink.foreachAsync[(Block, GetBlockHeaderResult)](1) {
        case (block, blockHeaderResult) =>
          val processBlocksF = processBlock(block)

          processBlocksF.failed.foreach { case err =>
            val failedCount = atomicPrevCount.get
            atomicPrevCount.set(prevCount)
            logger.error(
              s"Processing blocks from bitcoind polling failed, range=[$prevCount, $failedCount]",
              err
            )
          }

          for {
            _ <- processBlocksF
            _ <- handleChainCallbacks(chainCallbacksOpt, blockHeaderResult)
          } yield ()
      }
    }

    val fetchAndProcessBlockSink: Sink[DoubleSha256DigestBE, Future[Done]] = {
      fetchBlocksFlow.toMat(processBlockSink)(Keep.right)
    }

    val (queue, doneF) = queueSource
      .mapAsync(parallelism = numParallelism) { (height: Int) =>
        bitcoind.getBlockHash(height)
      }
      .map { hash =>
        val _ = atomicPrevCount.incrementAndGet()
        hash
      }
      .toMat(fetchAndProcessBlockSink)(Keep.both)
      .run()
    logger.debug("Polling bitcoind for block count")

    val resF: Future[Unit] = for {
      count <- bitcoind.getBlockCount()
      retval <- {
        if (prevCount < count) {
          logger.info(
            s"Bitcoind has new block(s), requesting... ${count - prevCount} blocks"
          )
          val setSyncFlagF = setSyncingFlag(true, bitcoind, chainCallbacksOpt)
          setSyncFlagF.map { _ =>
            // use .tail so we don't process the previous block that we already did
            val range = prevCount.to(count).tail
            range.foreach(r => queue.offer(r))
          }
        } else if (prevCount > count) {
          Future.failed(
            new RuntimeException(
              s"Bitcoind is at a block height ($count) before the wallet's ($prevCount)"
            )
          )
        } else {
          logger.debug(s"In sync $prevCount count=$count")
          Future.unit
        }
      }
    } yield {
      queue.complete() // complete the stream after offering all heights we need to sync
      retval
    }
    resF.map(_ => Some(doneF))
  }

  def startBitcoindMempoolPolling(
      wallet: WalletApi,
      bitcoind: BitcoindRpcClient,
      interval: FiniteDuration = 10.seconds
  )(
      processTx: Transaction => Future[Unit]
  )(implicit system: ActorSystem, ec: ExecutionContext): Cancellable = {
    @volatile var prevMempool: Set[DoubleSha256DigestBE] =
      Set.empty[DoubleSha256DigestBE]

    def getDiffAndReplace(
        newMempool: Set[DoubleSha256DigestBE]
    ): Set[DoubleSha256DigestBE] =
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

        // don't want to execute these in parallel
        val processTxFlow = Sink.foreachAsync[Option[Transaction]](1) {
          case Some(tx) => processTx(tx)
          case None     => Future.unit
        }

        val res = for {
          mempool <- bitcoind.getRawMempoolTxIds().map(_.txids)
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
            s"Done processing ${newTxIds.size} new mempool transactions"
          )
          ()
        }
        res.onComplete(_ => processingMempool.set(false))
        res
      } else {
        logger.info(
          s"Skipping scanning the mempool since a previously scheduled task is still running"
        )
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

  /** Checks if bitcoind has all blocks for the headers it has seen on the
    * network
    */
  private def isBitcoindInSync(
      bitcoind: BitcoindRpcClient
  )(implicit ec: ExecutionContext): Future[Boolean] = {
    for {
      blockchainInfo <- bitcoind.getBlockChainInfo
    } yield {
      blockchainInfo.headers == blockchainInfo.blocks
    }
  }

}
