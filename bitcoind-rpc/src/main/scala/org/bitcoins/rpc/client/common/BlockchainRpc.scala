package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind.*
import org.bitcoins.commons.serializers.{JsonSerializers, JsonWriters}
import JsonWriters.DescriptorWrites
import org.bitcoins.commons.serializers.JsonSerializers.*
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.api.chain.db.{
  CompactFilterDb,
  CompactFilterHeaderDb,
  CompactFilterHeaderDbHelper
}
import org.bitcoins.core.api.chain.{ChainApi, ChainQueryApi}
import org.bitcoins.core.gcs.{BlockFilter, FilterHeader, FilterType}
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.core.protocol.script.descriptor.Descriptor
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.rpc.client.common.BitcoindVersion.{Unknown, V27, V28, V29}
import play.api.libs.json.*

import scala.concurrent.Future

/** RPC calls related to querying the state of the blockchain
  */
trait BlockchainRpc extends ChainApi { self: Client =>

  override def getBestBlockHash(): Future[DoubleSha256DigestBE] = {
    bitcoindCall[DoubleSha256DigestBE]("getbestblockhash")
  }

  def getBlock(headerHash: DoubleSha256DigestBE): Future[GetBlockResult] = {
    val isJsonObject = JsNumber(1)
    bitcoindCall[GetBlockResult](
      "getblock",
      List(JsString(headerHash.hex), isJsonObject)
    )
  }

  def getBlock(headerHash: DoubleSha256Digest): Future[GetBlockResult] = {
    getBlock(headerHash.flip)
  }

  def getBlockChainInfo: Future[GetBlockChainInfoResult] = {
    self.version.flatMap {
      case V27 | Unknown =>
        bitcoindCall[GetBlockChainInfoResultPostV23]("getblockchaininfo")
      case V28 | V29 =>
        bitcoindCall[GetBlockChainInfoResultPostV27]("getblockchaininfo")
    }

  }

  override def getBlockCount(): Future[Int] = {
    bitcoindCall[Int]("getblockcount")
  }

  def getBlockHash(height: Int): Future[DoubleSha256DigestBE] = {
    bitcoindCall[DoubleSha256DigestBE]("getblockhash", List(JsNumber(height)))
  }

  def getBlockHeader(
      headerHash: DoubleSha256DigestBE
  ): Future[GetBlockHeaderResult] = {
    bitcoindCall[GetBlockHeaderResult](
      "getblockheader",
      List(JsString(headerHash.hex), JsBoolean(true))
    )
  }

  def getBlockHeader(
      headerHash: DoubleSha256Digest
  ): Future[GetBlockHeaderResult] = {
    getBlockHeader(headerHash.flip)
  }

  def getBlockHeaderRaw(
      headerHash: DoubleSha256DigestBE
  ): Future[BlockHeader] = {
    bitcoindCall[BlockHeader](
      "getblockheader",
      List(JsString(headerHash.hex), JsBoolean(false))
    )
  }

  def getBlockHeaderRaw(headerHash: DoubleSha256Digest): Future[BlockHeader] = {
    getBlockHeaderRaw(headerHash.flip)
  }

  def getBlockRaw(headerHash: DoubleSha256DigestBE): Future[Block] = {
    bitcoindCall[Block]("getblock", List(JsString(headerHash.hex), JsNumber(0)))
  }

  def getBlockRaw(headerHash: DoubleSha256Digest): Future[Block] = {
    getBlockRaw(headerHash.flip)
  }

  def getBlockWithTransactions(
      headerHash: DoubleSha256DigestBE
  ): Future[GetBlockWithTransactionsResultV22] = {
    val isVerboseJsonObject = JsNumber(2)
    bitcoindCall[GetBlockWithTransactionsResultV22](
      "getblock",
      List(JsString(headerHash.hex), isVerboseJsonObject)
    )
  }

  def getBlockWithTransactions(
      headerHash: DoubleSha256Digest
  ): Future[GetBlockWithTransactionsResult] = {
    getBlockWithTransactions(headerHash.flip)
  }

  def getChainTips: Future[Vector[ChainTip]] = {
    bitcoindCall[Vector[ChainTip]]("getchaintips")
  }

  def getChainTxStats: Future[GetChainTxStatsResult] =
    getChainTxStats(None, None)

  private def getChainTxStats(
      blocks: Option[Int],
      blockHash: Option[DoubleSha256DigestBE]
  ): Future[GetChainTxStatsResult] = {
    val params =
      if (blocks.isEmpty) {
        List.empty
      } else if (blockHash.isEmpty) {
        List(JsNumber(blocks.get))
      } else {
        List(JsNumber(blocks.get), JsString(blockHash.get.hex))
      }
    bitcoindCall[GetChainTxStatsResult]("getchaintxstats", params)
  }

  def getChainTxStats(blocks: Int): Future[GetChainTxStatsResult] =
    getChainTxStats(Some(blocks), None)

  def getChainTxStats(
      blocks: Int,
      blockHash: DoubleSha256DigestBE
  ): Future[GetChainTxStatsResult] =
    getChainTxStats(Some(blocks), Some(blockHash))

  def getChainTxStats(
      blocks: Int,
      blockHash: DoubleSha256Digest
  ): Future[GetChainTxStatsResult] =
    getChainTxStats(Some(blocks), Some(blockHash.flip))

  def getDifficulty: Future[BigDecimal] = {
    bitcoindCall[BigDecimal]("getdifficulty")
  }

  def invalidateBlock(blockHash: DoubleSha256DigestBE): Future[Unit] = {
    bitcoindCall[Unit]("invalidateblock", List(JsString(blockHash.hex)))
  }

  def invalidateBlock(blockHash: DoubleSha256Digest): Future[Unit] = {
    invalidateBlock(blockHash.flip)
  }

  def listSinceBlock: Future[ListSinceBlockResult] = listSinceBlock(None)

  def listSinceBlock(
      headerHash: Option[DoubleSha256DigestBE] = None,
      confirmations: Int = 1,
      includeWatchOnly: Boolean = false
  ): Future[ListSinceBlockResult] = {
    val params =
      if (headerHash.isEmpty) {
        List.empty
      } else {
        List(
          JsString(headerHash.get.hex),
          JsNumber(confirmations),
          JsBoolean(includeWatchOnly)
        )
      }
    bitcoindCall[ListSinceBlockResult]("listsinceblock", params)
  }

  def listSinceBlock(
      headerHash: DoubleSha256DigestBE
  ): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash))

  def listSinceBlock(
      headerHash: DoubleSha256DigestBE,
      confirmations: Int
  ): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash), confirmations)

  def listSinceBlock(
      headerHash: DoubleSha256DigestBE,
      includeWatchOnly: Boolean
  ): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash), includeWatchOnly = includeWatchOnly)

  def listSinceBlock(
      headerHash: DoubleSha256DigestBE,
      confirmations: Int,
      includeWatchOnly: Boolean
  ): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash), confirmations, includeWatchOnly)

  def listSinceBlock(
      headerHash: DoubleSha256Digest
  ): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash.flip))

  def listSinceBlock(
      headerHash: DoubleSha256Digest,
      confirmations: Int
  ): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash.flip), confirmations)

  def listSinceBlock(
      headerHash: DoubleSha256Digest,
      includeWatchOnly: Boolean
  ): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash.flip), includeWatchOnly = includeWatchOnly)

  def listSinceBlock(
      headerHash: DoubleSha256Digest,
      confirmations: Int,
      includeWatchOnly: Boolean
  ): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash.flip), confirmations, includeWatchOnly)

  def listTransactions(
      account: String = "*",
      count: Int = 10,
      skip: Int = 0,
      includeWatchOnly: Boolean = false
  ): Future[Vector[ListTransactionsResult]] = {
    bitcoindCall[Vector[ListTransactionsResult]](
      "listtransactions",
      List(
        JsString(account),
        JsNumber(count),
        JsNumber(skip),
        JsBoolean(includeWatchOnly)
      )
    )
  }

  def pruneBlockChain(height: Int): Future[Int] = {
    bitcoindCall[Int]("pruneblockchain", List(JsNumber(height)))
  }

  def rescanBlockChain(): Future[RescanBlockChainResult] =
    rescanBlockChain(None, None)

  private def rescanBlockChain(
      start: Option[Int],
      stop: Option[Int]
  ): Future[RescanBlockChainResult] = {
    val params =
      if (start.isEmpty) {
        List.empty
      } else if (stop.isEmpty) {
        List(JsNumber(start.get))
      } else {
        List(JsNumber(start.get), JsNumber(stop.get))
      }
    bitcoindCall[RescanBlockChainResult]("rescanblockchain", params)
  }

  def rescanBlockChain(start: Int): Future[RescanBlockChainResult] =
    rescanBlockChain(Some(start), None)

  def rescanBlockChain(start: Int, stop: Int): Future[RescanBlockChainResult] =
    rescanBlockChain(Some(start), Some(stop))

  def preciousBlock(headerHash: DoubleSha256DigestBE): Future[Unit] = {
    bitcoindCall[Unit]("preciousblock", List(JsString(headerHash.hex)))
  }

  def preciousBlock(headerHash: DoubleSha256Digest): Future[Unit] = {
    preciousBlock(headerHash.flip)
  }

  def verifyChain(level: Int = 3, blocks: Int = 6): Future[Boolean] = {
    bitcoindCall[Boolean](
      "verifychain",
      List(JsNumber(level), JsNumber(blocks))
    )
  }

  /** Waits for the validation interface queue to catch up on everything that
    * was there when we entered this function
    * @see
    *   [[https://github.com/bitcoin/bitcoin/issues/27085]]
    * @return
    */
  def syncWithValidationInterfaceQueue(): Future[Unit] = {
    bitcoindCall[Unit](command = "syncwithvalidationinterfacequeue", List.empty)
  }

  /** This is needed because we need the block hash to create a GolombFilter. We
    * use an intermediary data type to hold our data so we can add the block
    * hash we were given after the RPC call
    */
  private case class TempBlockFilterResult(
      filter: String,
      header: DoubleSha256DigestBE
  )

  implicit private val tempBlockFilterResultReads
      : Reads[TempBlockFilterResult] =
    Json.reads[TempBlockFilterResult]

  def getBlockFilter(
      blockhash: DoubleSha256DigestBE,
      filtertype: FilterType
  ): Future[GetBlockFilterResult] = {
    bitcoindCall[TempBlockFilterResult](
      "getblockfilter",
      List(JsString(blockhash.hex), JsString(filtertype.toString.toLowerCase))
    )
      .map { tempBlockFilterResult =>
        GetBlockFilterResult(
          BlockFilter.fromHex(tempBlockFilterResult.filter, blockhash.flip),
          tempBlockFilterResult.header
        )
      }
  }

  override def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int
  ): Future[Vector[ChainQueryApi.FilterResponse]] = {
    val allHeights = startHeight.to(endHeight)

    def f(range: Vector[Int]): Future[Vector[FilterResponse]] = {
      val filterFs = Future.traverse(range) { height =>
        for {
          hash <- getBlockHash(height)
          filter <- getBlockFilter(hash, FilterType.Basic)
        } yield {
          FilterResponse(filter.filter, hash, height)
        }
      }
      filterFs
    }

    FutureUtil.batchAndSyncExecute(
      elements = allHeights.toVector,
      f = f,
      batchSize = FutureUtil.getParallelism
    )
  }

  override def getFilterCount(): Future[Int] = getBlockCount()

  override def getFilterHeaderCount(): Future[Int] = getBlockCount()

  override def getBestFilterHeader(): Future[Option[CompactFilterHeaderDb]] = {
    for {
      height <- getFilterHeaderCount()
      blockHash <- getBlockHash(height)
      fhOpt <- getFilterHeader(blockHash)
    } yield fhOpt
  }

  override def getFilterHeader(
      blockHash: DoubleSha256DigestBE
  ): Future[Option[CompactFilterHeaderDb]] = {
    for {
      blockHeaderOpt <- getHeader(blockHash)
      (filterOpt, prevFilterOpt) <- blockHeaderOpt match {
        case Some(blockHeader) =>
          val fOptF = getFilter(blockHeader.hashBE)
          val prevOptF = getFilter(blockHeader.previousBlockHashBE)
          fOptF.flatMap(f => prevOptF.map(p => (f, p)))
        case None => Future.successful((None, None))
      }
    } yield {
      (filterOpt, prevFilterOpt) match {
        case (Some(filter), Some(prevFilter)) =>
          val fh = FilterHeader(filter.hashBE, prevFilter.hashBE)
          val c = CompactFilterHeaderDbHelper.fromFilterHeader(
            filterHeader = fh,
            blockHash = blockHash,
            height = filter.height
          )
          Some(c)
        case (Some(filter), None) =>
          // must be genesis filter
          val fh = FilterHeader(filter.hashBE, DoubleSha256DigestBE.empty)
          val c = CompactFilterHeaderDbHelper.fromFilterHeader(
            filterHeader = fh,
            blockHash = blockHash,
            height = filter.height
          )
          Some(c)
        case (None, Some(_)) =>
          // could find previous filter, but couldn't find compact filter?
          None
        case (None, None) => None
      }
    }
  }

  override def getFilterHeadersAtHeight(
      height: Int
  ): Future[Vector[CompactFilterHeaderDb]] = {
    getBlockHash(height)
      .flatMap { blockHashBE =>
        getFilterHeader(blockHashBE)
      }
      .map(_.toVector)
  }

  override def getFilter(
      hash: DoubleSha256DigestBE
  ): Future[Option[CompactFilterDb]] = {
    for {
      header <- getBlockHeader(hash)
      filter <- getBlockFilter(hash, FilterType.Basic)
    } yield Some(filter.filterDb(header.height, header.hash))
  }

  override def getFiltersAtHeight(
      height: Int
  ): Future[Vector[CompactFilterDb]] = {
    for {
      hash <- getBlockHash(height)
      filter <- getBlockFilter(hash, FilterType.Basic)
    } yield Vector(filter.filterDb(height, hash))
  }

  override def getBestFilter(): Future[Option[CompactFilterDb]] = {
    for {
      filterCount <- getFilterCount()
      blockHashBE <- getBlockHash(filterCount)
      filterOpt <- getFilter(blockHashBE)
    } yield {
      filterOpt
    }
  }

  def scanBlocks(request: ScanBlocksRequest): Future[ScanBlocksResult] = {
    bitcoindCall("scanblocks", request.params)(
      JsonSerializers.ScanBlocksResultReads)
  }

  def getChainStates(): Future[ChainStateResult] = {
    bitcoindCall("getchainstates")(JsonSerializers.chainStateResultReads)
  }

  def getDescriptorActivity(
      blockHashes: Vector[DoubleSha256DigestBE],
      scanobjects: Vector[Descriptor],
      includeMempool: Boolean = true): Future[GetDescriptorActivityResult] = {
    bitcoindCall("getdescriptoractivity",
                 List(Json.toJson(blockHashes), Json.toJson(scanobjects)))(
      JsonSerializers.getDescriptorActivityResultReads)
  }
}
