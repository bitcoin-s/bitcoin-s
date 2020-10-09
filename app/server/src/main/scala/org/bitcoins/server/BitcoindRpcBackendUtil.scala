package org.bitcoins.server

import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorSystem, Cancellable}
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.{BitcoinSLogger, FutureUtil}
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.wallet.Wallet
import org.bitcoins.zmq.ZMQSubscriber

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

/** Useful utilities to use in the wallet project for syncing things against bitcoind */
object BitcoindRpcBackendUtil extends BitcoinSLogger {

  def createWalletWithBitcoindCallbacks(
      bitcoind: BitcoindRpcClient,
      wallet: Wallet)(implicit ec: ExecutionContext): Wallet = {
    // Kill the old wallet
    wallet.stopWalletThread()

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

  def createZMQWalletCallbacks(wallet: Wallet)(implicit
      bitcoindRpcConf: BitcoindRpcAppConfig): ZMQSubscriber = {
    require(bitcoindRpcConf.zmqPortOpt.isDefined,
            "Must have the zmq port defined to setup ZMQ callbacks")
    val zmqSocket =
      new InetSocketAddress("tcp://127.0.0.1", bitcoindRpcConf.zmqPortOpt.get)

    val rawTxListener: Option[Transaction => Unit] = Some {
      { tx: Transaction =>
        logger.debug(s"Received tx ${tx.txIdBE}, processing")
        wallet.processTransaction(tx, None)
        ()
      }
    }

    val rawBlockListener: Option[Block => Unit] = Some {
      { block: Block =>
        logger.debug(s"Received block ${block.blockHeader.hashBE}, processing")
        wallet.processBlock(block)
        ()
      }
    }

    new ZMQSubscriber(socket = zmqSocket,
                      hashTxListener = None,
                      hashBlockListener = None,
                      rawTxListener = rawTxListener,
                      rawBlockListener = rawBlockListener)
  }

  private def getNodeApiWalletCallback(
      bitcoindRpcClient: BitcoindRpcClient,
      walletF: Future[Wallet])(implicit ec: ExecutionContext): NodeApi = {
    new NodeApi {

      override def downloadBlocks(
          blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = {
        logger.info(s"Fetching ${blockHashes.length} hashes from bitcoind")
        val f: Vector[DoubleSha256Digest] => Future[Wallet] = { hashes =>
          val blocksF =
            FutureUtil.sequentially(hashes)(bitcoindRpcClient.getBlockRaw)

          val updatedWalletF = for {
            blocks <- blocksF
            wallet <- walletF
            processedWallet <- {
              FutureUtil.foldLeftAsync(wallet, blocks) {
                case (wallet, block) =>
                  wallet.processBlock(block)
              }
            }
          } yield processedWallet

          updatedWalletF
        }

        val batchSize = 25
        val batchedExecutedF = FutureUtil.batchExecute(elements = blockHashes,
                                                       f = f,
                                                       init = Vector.empty,
                                                       batchSize = batchSize)

        batchedExecutedF.map { _ =>
          logger.info(
            s"Done fetching ${blockHashes.length} hashes from bitcoind")
          ()
        }
      }

      /**
        * Broadcasts the given transaction over the P2P network
        */
      override def broadcastTransaction(
          transaction: Transaction): Future[Unit] = {
        bitcoindRpcClient.sendRawTransaction(transaction).map(_ => ())
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
    val atomicPrevCount: AtomicReference[Int] = new AtomicReference(startCount)
    system.scheduler.scheduleWithFixedDelay(0.seconds, interval) { () =>
      {
        logger.debug("Polling bitcoind for block count")
        bitcoind.getBlockCount.flatMap { count =>
          val prevCount = atomicPrevCount.get()
          if (prevCount < count) {
            logger.debug("Bitcoind has new block(s), requesting...")

            // use .tail so we don't process the previous block that we already did
            val range = prevCount.to(count).tail

            val hashFs =
              range.map(bitcoind.getBlockHash(_).map(_.flip))

            val oldPrevCount = prevCount
            atomicPrevCount.set(count)

            val requestsBlocksF = for {
              hashes <- Future.sequence(hashFs)
              _ <- wallet.nodeApi.downloadBlocks(hashes.toVector)
            } yield logger.debug("Successfully polled bitcoind for new blocks")

            requestsBlocksF.onComplete {
              case Success(_) => ()
              case Failure(err) =>
                atomicPrevCount.set(oldPrevCount)
                logger.error("Requesting blocks from bitcoind polling failed",
                             err)
            }

            requestsBlocksF
          } else if (prevCount > count) {
            Future.failed(new RuntimeException(
              s"Bitcoind is at a block height ($count) before the wallet's ($prevCount)"))
          } else FutureUtil.unit
        }
        ()
      }
    }
  }
}
