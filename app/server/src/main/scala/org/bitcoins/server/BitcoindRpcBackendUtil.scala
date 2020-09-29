package org.bitcoins.server

import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.{BitcoinSLogger, FutureUtil}
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.wallet.Wallet

import scala.concurrent.{ExecutionContext, Future}

/** Useful utilities to use in the wallet project for syncing things against bitcoind */
object BitcoindRpcBackendUtil extends BitcoinSLogger {

  def getChainQueryApi(bitcoind: BitcoindRpcClient): ChainQueryApi = {
    new ChainQueryApi {

      override def getBlockHeight(
          blockHash: DoubleSha256DigestBE): Future[Option[Int]] =
        bitcoind.getBlockHeight(blockHash)

      override def getBestBlockHash(): Future[DoubleSha256DigestBE] = {
        bitcoind.getBestBlockHash
      }

      override def getNumberOfConfirmations(
          blockHashOpt: DoubleSha256DigestBE): Future[Option[Int]] = {
        bitcoind.getNumberOfConfirmations(blockHashOpt)
      }

      override def getFilterCount: Future[Int] = {
        bitcoind.getFilterCount
      }

      override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
        bitcoind.getHeightByBlockStamp(blockStamp)

      override def epochSecondToBlockHeight(time: Long): Future[Int] =
        Future.successful(0)

      override def getFiltersBetweenHeights(
          startHeight: Int,
          endHeight: Int): Future[Vector[FilterResponse]] =
        bitcoind.getFiltersBetweenHeights(startHeight, endHeight)
    }
  }

  def getNodeApiWalletCallback(
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
}
