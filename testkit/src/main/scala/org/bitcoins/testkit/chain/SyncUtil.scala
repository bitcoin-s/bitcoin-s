package org.bitcoins.testkit.chain

import org.bitcoins.chain.blockchain.sync.FilterWithHeaderHash
import org.bitcoins.core.api.ChainQueryApi
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.gcs.{FilterType, GolombFilter}
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.rpc.jsonmodels.GetBlockFilterResult

import scala.concurrent.{ExecutionContext, Future}

/** Useful utilities to use in the chain project for syncing things against bitcoind */
abstract class SyncUtil {

  /** Creates a function that will retrun bitcoin's best block hash when called */
  def getBestBlockHashFunc(
      bitcoind: BitcoindRpcClient): () => Future[DoubleSha256DigestBE] = { () =>
    bitcoind.getBestBlockHash
  }

  /** Creates a function that you can pass a hash to and it returns the block header */
  def getBlockHeaderFunc(bitcoind: BitcoindRpcClient)(
      implicit ec: ExecutionContext): DoubleSha256DigestBE => Future[
    BlockHeader] = { hash: DoubleSha256DigestBE =>
    bitcoind.getBlockHeader(hash).map(_.blockHeader)
  }

  /** Creates a function that you can pass a block header to and it's return's it's [[GolombFilter]] */
  def getFilterFunc(bitcoind: BitcoindV19RpcClient, filterType: FilterType)(
      implicit ec: ExecutionContext): BlockHeader => Future[
    FilterWithHeaderHash] = {
    case header: BlockHeader =>
      val prevFilterResultF =
        bitcoind.getBlockFilter(header.hashBE, filterType)
      prevFilterResultF.map {
        case GetBlockFilterResult(filter, header) =>
          FilterWithHeaderHash(filter, header)
      }
  }

  def getChainQueryApi(bitcoindV19RpcClient: BitcoindV19RpcClient)(
      implicit ec: ExecutionContext): ChainQueryApi = {
    new ChainQueryApi {

      /** Gets the height of the given block */
      override def getBlockHeight(
          blockHash: DoubleSha256DigestBE): Future[Option[Int]] = {
        bitcoindV19RpcClient
          .getBlockHeader(blockHash)
          .map(b => Some(b.height))
      }

      /** Gets the hash of the block that is what we consider "best" */
      override def getBestBlockHash(): Future[DoubleSha256DigestBE] = {
        bitcoindV19RpcClient.getBestBlockHash
      }

      /** Gets number of confirmations for the given block hash */
      override def getNumberOfConfirmations(
          blockHashOpt: DoubleSha256DigestBE): Future[Option[Int]] = {
        bitcoindV19RpcClient.getBlock(blockHashOpt).map { b =>
          Some(b.confirmations)
        }
      }

      /** Gets the number of compact filters in the database */
      override def getFilterCount: Future[Int] = {
        //filter count should be same as block height?
        bitcoindV19RpcClient.getBlockCount
      }

      /** Returns the block height of the given block stamp */
      override def getHeightByBlockStamp(
          blockStamp: BlockStamp): Future[Int] = {
        blockStamp match {
          case BlockStamp.BlockHash(hash) => getBlockHeight(hash).map(_.get)
          case BlockStamp.BlockHeight(height) =>
            Future.failed(
              new RuntimeException(
                s"Cannot query bitcoind by height=${height}"))
          case BlockStamp.BlockTime(time) =>
            Future.failed(new RuntimeException(s"Cannot query by block time"))
        }
      }

      override def getFiltersBetweenHeights(
          startHeight: Int,
          endHeight: Int): Future[Vector[ChainQueryApi.FilterResponse]] = {
        //how to query filters by height with bitcoind?
        Future.failed(
          new RuntimeException(s"Cannot query filters by height with bitcoind"))
      }
    }
  }
}

object SyncUtil extends SyncUtil
