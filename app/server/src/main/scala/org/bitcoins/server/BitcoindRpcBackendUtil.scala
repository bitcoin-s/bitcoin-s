package org.bitcoins.server

import akka.Done
import akka.actor.{ActorSystem, Cancellable}
import akka.stream.scaladsl.{Keep, Sink, Source}
import grizzled.slf4j.Logging
import org.bitcoins.chain.{ChainCallbacks}

import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.WalletApi
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.V19BlockFilterRpc
import org.bitcoins.rpc.config.ZmqConfig
import org.bitcoins.wallet.Wallet
import org.bitcoins.zmq.ZMQSubscriber

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, Promise}

/** Useful utilities to use in the wallet project for syncing things against bitcoind */
object BitcoindRpcBackendUtil extends Logging {

  /** Has the wallet process all the blocks it has not seen up until bitcoind's chain tip */
  def syncWalletToBitcoind(bitcoind: BitcoindRpcClient, wallet: Wallet)(implicit
      system: ActorSystem): Future[Unit] = {
    logger.info("Syncing wallet to bitcoind")
    import system.dispatcher
    for {
      bitcoindHeight <- bitcoind.getBlockCount
      walletStateOpt <- wallet.getSyncDescriptorOpt()
      _ = logger.info(
        s"bitcoindHeight=$bitcoindHeight walletStateOpt=$walletStateOpt")
      _ <- walletStateOpt match {
        case None =>
          for {
            txDbs <- wallet.listTransactions()
            lastConfirmedOpt = txDbs.filter(_.blockHashOpt.isDefined).lastOption
            _ <- lastConfirmedOpt match {
              case None =>
                for {
                  _ <- doSync(walletHeight = bitcoindHeight - 1,
                              bitcoindHeight = bitcoindHeight,
                              bitcoind = bitcoind,
                              wallet = wallet)
                } yield ()
              case Some(txDb) =>
                for {
                  heightOpt <- bitcoind.getBlockHeight(txDb.blockHashOpt.get)
                  _ <- heightOpt match {
                    case Some(height) =>
                      logger.info(
                        s"Last tx occurred at block $height, syncing from there")
                      doSync(height, bitcoindHeight, bitcoind, wallet)
                    case None => Future.unit
                  }
                } yield ()
            }
          } yield ()
        case Some(syncHeight) =>
          doSync(syncHeight.height, bitcoindHeight, bitcoind, wallet)
      }
    } yield ()
  }

  /** Helper method to sync the wallet until the bitcoind height */
  private def doSync(
      walletHeight: Int,
      bitcoindHeight: Int,
      bitcoind: BitcoindRpcClient,
      wallet: Wallet)(implicit system: ActorSystem): Future[Wallet] = {
    if (walletHeight > bitcoindHeight) {
      val msg = s"Bitcoind and wallet are in incompatible states, " +
        s"wallet height: $walletHeight, bitcoind height: $bitcoindHeight"
      logger.error(msg)
      Future.failed(new RuntimeException(msg))
    } else {
      logger.info(s"Syncing from $walletHeight to $bitcoindHeight")

      import system.dispatcher

      val hasFiltersF = bitcoind
        .getFilter(wallet.walletConfig.chain.genesisHashBE)
        .map(_ => true)
        .recover { case _: Throwable => false }

      val blockRange = walletHeight.to(bitcoindHeight).tail
      val numParallelism = Runtime.getRuntime.availableProcessors()
      logger.info(s"Syncing ${blockRange.size} blocks")
      logger.info(s"Fetching block hashes")
      val hashFs = Source(blockRange)
        .mapAsync(numParallelism) {
          bitcoind
            .getBlockHash(_)
            .map(_.flip)
        }
        .toMat(Sink.seq)(Keep.right)
        .run()
      for {
        hashes <- hashFs.map(_.toVector)
        hasFilters <- hasFiltersF
        _ <- {
          if (hasFilters) {
            filterSync(hashes, bitcoind.asInstanceOf[V19BlockFilterRpc], wallet)
          } else wallet.nodeApi.downloadBlocks(hashes)
        }
      } yield wallet
    }
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

    val pairedWallet = Wallet(
      nodeApi =
        BitcoindRpcBackendUtil.buildBitcoindNodeApi(bitcoind,
                                                    walletCallbackP.future,
                                                    chainCallbacksOpt),
      chainQueryApi = bitcoind,
      feeRateApi = wallet.feeRateApi
    )(wallet.walletConfig, wallet.ec)

    walletCallbackP.success(pairedWallet)

    pairedWallet
  }

  def startZMQWalletCallbacks(wallet: Wallet, zmqConfig: ZmqConfig): Unit = {
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
    )(wallet.walletConfig, wallet.dlcConfig, wallet.ec)

    walletCallbackP.success(pairedWallet)

    pairedWallet
  }

  private def filterSync(
      blockHashes: Vector[DoubleSha256Digest],
      bitcoindRpcClient: V19BlockFilterRpc,
      wallet: Wallet)(implicit system: ActorSystem): Future[Unit] = {
    import system.dispatcher

    logger.info("Starting filter sync")
    val start = System.currentTimeMillis()

    val numParallelism = Runtime.getRuntime.availableProcessors()
    val runStream: Future[Done] = Source(blockHashes)
      .mapAsync(parallelism = numParallelism) { hash =>
        bitcoindRpcClient.getBlockFilter(hash.flip, FilterType.Basic).map {
          res => (hash, res.filter)
        }
      }
      .batch(1000, filter => Vector(filter))(_ :+ _)
      .foldAsync(wallet) { case (wallet, filterRes) =>
        wallet.processCompactFilters(filterRes)
      }
      .run()
    runStream.map { _ =>
      logger.info(s"Synced ${blockHashes.size} filters, it took ${System
        .currentTimeMillis() - start}ms")
      logger.info("We are synced!")
    }
  }

  /** Creates an anonymous [[NodeApi]] that downloads blocks using
    * akka streams from bitcoind, and then calls [[Wallet.processBlock]]
    */
  private def buildBitcoindNodeApi(
      bitcoindRpcClient: BitcoindRpcClient,
      walletF: Future[Wallet],
      chainCallbacksOpt: Option[ChainCallbacks])(implicit
      system: ActorSystem): NodeApi = {
    import system.dispatcher
    new NodeApi {

      override def downloadBlocks(
          blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = {
        logger.info(s"Fetching ${blockHashes.length} blocks from bitcoind")
        val numParallelism = Runtime.getRuntime.availableProcessors()
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
                val executeCallbackF: Future[Wallet] = blockProcessedF.flatMap {
                  wallet =>
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
    * @param interval The amount of time between polls, this should not be too aggressive
    *                 as the wallet will need to process the new blocks
    */
  def startBitcoindBlockPolling(
      wallet: WalletApi,
      bitcoind: BitcoindRpcClient,
      interval: FiniteDuration = 10.seconds)(implicit
      system: ActorSystem,
      ec: ExecutionContext): Future[Cancellable] = {
    val walletSyncStateF = wallet.getSyncState()
    val resultF: Future[Cancellable] = for {
      walletSyncState <- walletSyncStateF
    } yield {
      val numParallelism = Runtime.getRuntime.availableProcessors()
      val atomicPrevCount: AtomicInteger = new AtomicInteger(
        walletSyncState.height)
      system.scheduler.scheduleWithFixedDelay(0.seconds, interval) { () =>
        {
          logger.trace("Polling bitcoind for block count")
          bitcoind.getBlockCount.flatMap { count =>
            val prevCount = atomicPrevCount.get()
            if (prevCount < count) {
              logger.info(
                s"Bitcoind has new block(s), requesting... ${count - prevCount} blocks")

              // use .tail so we don't process the previous block that we already did
              val range = prevCount.to(count).tail
              val hashFs: Future[Seq[DoubleSha256Digest]] = Source(range)
                .mapAsync(parallelism = numParallelism) { height =>
                  bitcoind.getBlockHash(height).map(_.flip)
                }
                .map { hash =>
                  val _ = atomicPrevCount.incrementAndGet()
                  hash
                }
                .toMat(Sink.seq)(Keep.right)
                .run()

              val requestsBlocksF = for {
                hashes <- hashFs
                _ <- wallet.nodeApi.downloadBlocks(hashes.toVector)
              } yield logger.debug(
                "Successfully polled bitcoind for new blocks")

              requestsBlocksF.failed.foreach { case err =>
                val failedCount = atomicPrevCount.get
                atomicPrevCount.set(prevCount)
                logger.error(
                  s"Requesting blocks from bitcoind polling failed, range=[$prevCount, $failedCount]",
                  err)
              }

              requestsBlocksF
            } else if (prevCount > count) {
              Future.failed(new RuntimeException(
                s"Bitcoind is at a block height ($count) before the wallet's ($prevCount)"))
            } else {
              logger.debug(s"In sync $prevCount count=$count")
              Future.unit
            }
          }
          ()
        }
      }
    }

    resultF
  }
}
