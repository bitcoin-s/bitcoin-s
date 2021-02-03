package org.bitcoins.testkit.chain

import org.bitcoins.chain.blockchain.sync.FilterWithHeaderHash
import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockFilterResult
import org.bitcoins.core.api.node
import org.bitcoins.core.api.node.{NodeApi, NodeChainQueryApi}
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.{BitcoinSLogger, FutureUtil}
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.wallet.Wallet
import org.bitcoins.wallet.sync.WalletSync

import scala.concurrent.{ExecutionContext, Future}

/** Useful utilities to use in the chain project for syncing things against bitcoind */
abstract class SyncUtil extends BitcoinSLogger {

  /** Creates a function that will retrun bitcoin's best block hash when called */
  def getBestBlockHashFunc(
      bitcoind: BitcoindRpcClient): () => Future[DoubleSha256DigestBE] = { () =>
    bitcoind.getBestBlockHash
  }

  /** Creates a function that you can pass a hash to and it returns the block header */
  def getBlockHeaderFunc(bitcoind: BitcoindRpcClient)(implicit
      ec: ExecutionContext): DoubleSha256DigestBE => Future[BlockHeader] = {
    hash: DoubleSha256DigestBE =>
      bitcoind.getBlockHeader(hash).map(_.blockHeader)
  }

  /** Creates a function that you can pass a block header to and it's return's it's [[GolombFilter]] */
  def getFilterFunc(
      bitcoind: BitcoindV19RpcClient,
      filterType: FilterType)(implicit
      ec: ExecutionContext): BlockHeader => Future[FilterWithHeaderHash] = {
    case header: BlockHeader =>
      val prevFilterResultF =
        bitcoind.getBlockFilter(header.hashBE, filterType)
      prevFilterResultF.map { case GetBlockFilterResult(filter, header) =>
        FilterWithHeaderHash(filter, header)
      }
  }

  def getBlockFunc(
      bitcoind: BitcoindRpcClient): DoubleSha256DigestBE => Future[Block] = {
    bitcoind.getBlockRaw(_: DoubleSha256DigestBE)
  }

  def getNodeApi(bitcoindRpcClient: BitcoindRpcClient)(implicit
      ec: ExecutionContext): NodeApi = {
    new NodeApi {

      override def broadcastTransaction(
          transaction: Transaction): Future[Unit] = {
        bitcoindRpcClient.sendRawTransaction(transaction).map(_ => ())
      }

      /** Request the underlying node to download the given blocks from its peers and feed the blocks to [[org.bitcoins.node.NodeCallbacks]].
        */
      override def downloadBlocks(
          blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = {
        logger.info(s"Fetching ${blockHashes.length} hashes from bitcoind")
        val f: Vector[DoubleSha256Digest] => Future[Vector[Unit]] = {
          case hashes =>
            val blocks: Vector[Future[Unit]] = hashes.map {
              bitcoindRpcClient
                .getBlockRaw(_)
                .map(_ => ())
            }
            Future.sequence(blocks)
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
    }
  }

  def getNodeApiWalletCallback(
      bitcoindRpcClient: BitcoindRpcClient,
      walletF: Future[Wallet])(implicit ec: ExecutionContext): NodeApi = {
    new NodeApi {

      /** Request the underlying node to download the given blocks from its peers and feed the blocks to [[org.bitcoins.node.NodeCallbacks]].
        */
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
              FutureUtil.foldLeftAsync(wallet, blocks) { case (wallet, block) =>
                wallet.processBlock(block)
              }
            }
          } yield processedWallet

          updatedWalletF
        }

        val batchSize = 25
        val batchedExecutedF = {
          for {
            wallet <- walletF
            updatedWallet <-
              FutureUtil.batchExecute[DoubleSha256Digest, Wallet](
                elements = blockHashes,
                f = f,
                init = wallet,
                batchSize = batchSize)
          } yield updatedWallet

        }

        batchedExecutedF.map { _ =>
          logger.info(
            s"Done fetching ${blockHashes.length} hashes from bitcoind")
          ()
        }
      }

      /** Broadcasts the given transaction over the P2P network
        */
      override def broadcastTransaction(
          transaction: Transaction): Future[Unit] = {
        bitcoindRpcClient.sendRawTransaction(transaction).map(_ => ())
      }
    }
  }

  def getNodeChainQueryApi(bitcoind: BitcoindRpcClient)(implicit
      ec: ExecutionContext): NodeChainQueryApi = {
    val chainQuery = bitcoind
    val nodeApi = SyncUtil.getNodeApi(bitcoind)
    node.NodeChainQueryApi(nodeApi, chainQuery)
  }

  def getNodeChainQueryApiWalletCallback(
      bitcoind: BitcoindRpcClient,
      walletF: Future[Wallet])(implicit
      ec: ExecutionContext): NodeChainQueryApi = {
    val chainQuery = bitcoind
    val nodeApi =
      SyncUtil.getNodeApiWalletCallback(bitcoind, walletF)
    node.NodeChainQueryApi(nodeApi, chainQuery)
  }

  /** Syncs a wallet against bitcoind by retrieving full blocks and then calling
    * [[Wallet.processBlock()]]
    */
  def syncWalletFullBlocks(wallet: Wallet, bitcoind: BitcoindRpcClient)(implicit
      ec: ExecutionContext): Future[Wallet] = {
    WalletSync.syncFullBlocks(
      wallet = wallet,
      getBlockHeaderFunc = SyncUtil.getBlockHeaderFunc(bitcoind),
      getBestBlockHashFunc = SyncUtil.getBestBlockHashFunc(bitcoind),
      getBlockFunc = SyncUtil.getBlockFunc(bitcoind)
    )
  }
}

object SyncUtil extends SyncUtil
