package org.bitcoins.server

import akka.Done
import akka.actor.{ActorSystem, Cancellable}
import akka.stream.scaladsl.{Keep, Sink, Source}
import grizzled.slf4j.Logging
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.ZmqConfig
import org.bitcoins.wallet.Wallet
import org.bitcoins.zmq.ZMQSubscriber

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

/** Useful utilities to use in the wallet project for syncing things against bitcoind */
object BitcoindRpcBackendUtil extends Logging {

  /** Has the wallet process all the blocks it has not seen up until bitcoind's chain tip */
  def syncWalletToBitcoind(bitcoind: BitcoindRpcClient, wallet: Wallet)(implicit
      ec: ExecutionContext): Future[Unit] = {

    def doSync(walletHeight: Int, bitcoindHeight: Int): Future[Unit] = {
      if (walletHeight > bitcoindHeight) {
        Future.failed(new RuntimeException(
          s"Bitcoind and wallet are in incompatible states, " +
            s"wallet height: $walletHeight, bitcoind height: $bitcoindHeight"))
      } else {
        val blockRange = walletHeight.to(bitcoindHeight).tail

        logger.info(s"Syncing ${blockRange.size} blocks")

        val func: Vector[Int] => Future[Unit] = { range =>
          val hashFs =
            range.map(bitcoind.getBlockHash(_).map(_.flip))
          for {
            hashes <- Future.sequence(hashFs)
            _ <- wallet.nodeApi.downloadBlocks(hashes)
          } yield ()
        }

        FutureUtil
          .batchExecute(elements = blockRange.toVector,
                        f = func,
                        init = Vector.empty,
                        batchSize = 25)
          .map(_ => ())
      }
    }

    for {
      bitcoindHeight <- bitcoind.getBlockCount
      walletStateOpt <- wallet.getSyncDescriptorOpt()
      _ <- walletStateOpt match {
        case None =>
          for {
            txDbs <- wallet.listTransactions()
            lastConfirmedOpt = txDbs.filter(_.blockHashOpt.isDefined).lastOption
            _ <- lastConfirmedOpt match {
              case None => Future.unit
              case Some(txDb) =>
                for {
                  heightOpt <- bitcoind.getBlockHeight(txDb.blockHashOpt.get)
                  _ <- heightOpt match {
                    case Some(height) =>
                      logger.info(
                        s"Last tx occurred at block $height, syncing from there")
                      doSync(height, bitcoindHeight)
                    case None => Future.unit
                  }
                } yield ()
            }
          } yield ()
        case Some(syncHeight) =>
          doSync(syncHeight.height, bitcoindHeight)
      }
    } yield ()
  }

  def createWalletWithBitcoindCallbacks(
      bitcoind: BitcoindRpcClient,
      wallet: Wallet)(implicit system: ActorSystem): Wallet = {
    // We need to create a promise so we can inject the wallet with the callback
    // after we have created it into SyncUtil.getNodeApiWalletCallback
    // so we don't lose the internal state of the wallet
    val walletCallbackP = Promise[Wallet]()

    val pairedWallet = Wallet(
      keyManager = wallet.keyManager,
      nodeApi =
        BitcoindRpcBackendUtil.getNodeApiWalletCallback(bitcoind,
                                                        walletCallbackP.future),
      chainQueryApi = bitcoind,
      feeRateApi = wallet.feeRateApi,
      creationTime = wallet.keyManager.creationTime
    )(wallet.walletConfig, wallet.ec)

    walletCallbackP.success(pairedWallet)

    pairedWallet
  }

  def startZMQWalletCallbacks(wallet: Wallet)(implicit
      bitcoindRpcConf: BitcoindRpcAppConfig): Unit = {
    require(bitcoindRpcConf.zmqConfig != ZmqConfig.empty,
            "Must have the zmq raw configs defined to setup ZMQ callbacks")

    bitcoindRpcConf.zmqRawTx.foreach { zmq =>
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

    bitcoindRpcConf.zmqRawBlock.foreach { zmq =>
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

  private def getNodeApiWalletCallback(
      bitcoindRpcClient: BitcoindRpcClient,
      walletF: Future[Wallet])(implicit system: ActorSystem): NodeApi = {
    import system.dispatcher
    new NodeApi {

      override def downloadBlocks(
          blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = {
        logger.info(s"Fetching ${blockHashes.length} hashes from bitcoind")
        val numParallelism = Runtime.getRuntime.availableProcessors()
        walletF.flatMap { wallet =>
          val runStream: Future[Done] = Source(blockHashes)
            .mapAsync(parallelism = numParallelism) { hash =>
              bitcoindRpcClient.getBlockRaw(hash)
            }
            .foldAsync(wallet) { case (wallet, block) =>
              wallet.processBlock(block)
            }
            .run()
          runStream.map(_ => ())
        }
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
      wallet: Wallet,
      bitcoind: BitcoindRpcClient,
      startCount: Int,
      interval: FiniteDuration = 10.seconds)(implicit
      system: ActorSystem,
      ec: ExecutionContext): Cancellable = {
    val numParallelism = Runtime.getRuntime.availableProcessors()
    val atomicPrevCount: AtomicInteger = new AtomicInteger(startCount)
    system.scheduler.scheduleWithFixedDelay(0.seconds, interval) { () =>
      {
        logger.debug("Polling bitcoind for block count")
        bitcoind.getBlockCount.flatMap { count =>
          val prevCount = atomicPrevCount.get()
          if (prevCount < count) {
            logger.debug("Bitcoind has new block(s), requesting...")

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
            } yield logger.debug("Successfully polled bitcoind for new blocks")

            requestsBlocksF.onComplete {
              case Success(_) => ()
              case Failure(err) =>
                atomicPrevCount.set(prevCount)
                logger.error("Requesting blocks from bitcoind polling failed",
                             err)
            }

            requestsBlocksF
          } else if (prevCount > count) {
            Future.failed(new RuntimeException(
              s"Bitcoind is at a block height ($count) before the wallet's ($prevCount)"))
          } else Future.unit
        }
        ()
      }
    }
  }
}
