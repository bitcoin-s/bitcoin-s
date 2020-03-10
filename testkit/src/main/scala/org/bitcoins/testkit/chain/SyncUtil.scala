package org.bitcoins.testkit.chain

import org.bitcoins.chain.blockchain.sync.FilterWithHeaderHash
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.gcs.{FilterType, GolombFilter}
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
}

object SyncUtil extends SyncUtil
