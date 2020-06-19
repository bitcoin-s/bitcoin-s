package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import play.api.libs.json.{JsBoolean, JsNumber, JsString}

import scala.concurrent.Future

/**
  * RPC calls related to querying the state of the blockchain
  */
trait BlockchainRpc { self: Client =>

  def getBestBlockHash: Future[DoubleSha256DigestBE] = {
    bitcoindCall[DoubleSha256DigestBE]("getbestblockhash")
  }

  def getBlock(headerHash: DoubleSha256DigestBE): Future[GetBlockResult] = {
    val isJsonObject = JsNumber(1)
    bitcoindCall[GetBlockResult]("getblock",
                                 List(JsString(headerHash.hex), isJsonObject))
  }

  def getBlock(headerHash: DoubleSha256Digest): Future[GetBlockResult] = {
    getBlock(headerHash.flip)
  }

  def getBlockChainInfo: Future[GetBlockChainInfoResult] = {
    bitcoindCall[GetBlockChainInfoResult]("getblockchaininfo")
  }

  def getBlockCount: Future[Int] = {
    bitcoindCall[Int]("getblockcount")
  }

  def getBlockHash(height: Int): Future[DoubleSha256DigestBE] = {
    bitcoindCall[DoubleSha256DigestBE]("getblockhash", List(JsNumber(height)))
  }

  def getBlockHeader(
      headerHash: DoubleSha256DigestBE): Future[GetBlockHeaderResult] = {
    bitcoindCall[GetBlockHeaderResult](
      "getblockheader",
      List(JsString(headerHash.hex), JsBoolean(true)))
  }

  def getBlockHeader(
      headerHash: DoubleSha256Digest): Future[GetBlockHeaderResult] = {
    getBlockHeader(headerHash.flip)
  }

  def getBlockHeaderRaw(
      headerHash: DoubleSha256DigestBE): Future[BlockHeader] = {
    bitcoindCall[BlockHeader]("getblockheader",
                              List(JsString(headerHash.hex), JsBoolean(false)))
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

  def getBlockWithTransactions(headerHash: DoubleSha256DigestBE): Future[
    GetBlockWithTransactionsResult] = {
    val isVerboseJsonObject = JsNumber(2)
    bitcoindCall[GetBlockWithTransactionsResult](
      "getblock",
      List(JsString(headerHash.hex), isVerboseJsonObject))
  }

  def getBlockWithTransactions(headerHash: DoubleSha256Digest): Future[
    GetBlockWithTransactionsResult] = {
    getBlockWithTransactions(headerHash.flip)
  }

  def getChainTips: Future[Vector[ChainTip]] = {
    bitcoindCall[Vector[ChainTip]]("getchaintips")
  }

  def getChainTxStats: Future[GetChainTxStatsResult] =
    getChainTxStats(None, None)

  private def getChainTxStats(
      blocks: Option[Int],
      blockHash: Option[DoubleSha256DigestBE]): Future[
    GetChainTxStatsResult] = {
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
      blockHash: DoubleSha256DigestBE): Future[GetChainTxStatsResult] =
    getChainTxStats(Some(blocks), Some(blockHash))

  def getChainTxStats(
      blocks: Int,
      blockHash: DoubleSha256Digest): Future[GetChainTxStatsResult] =
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
      includeWatchOnly: Boolean = false): Future[ListSinceBlockResult] = {
    val params =
      if (headerHash.isEmpty) {
        List.empty
      } else {
        List(JsString(headerHash.get.hex),
             JsNumber(confirmations),
             JsBoolean(includeWatchOnly))
      }
    bitcoindCall[ListSinceBlockResult]("listsinceblock", params)
  }

  def listSinceBlock(
      headerHash: DoubleSha256DigestBE): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash))

  def listSinceBlock(
      headerHash: DoubleSha256DigestBE,
      confirmations: Int): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash), confirmations)

  def listSinceBlock(
      headerHash: DoubleSha256DigestBE,
      includeWatchOnly: Boolean): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash), includeWatchOnly = includeWatchOnly)

  def listSinceBlock(
      headerHash: DoubleSha256DigestBE,
      confirmations: Int,
      includeWatchOnly: Boolean): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash), confirmations, includeWatchOnly)

  def listSinceBlock(
      headerHash: DoubleSha256Digest): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash.flip))

  def listSinceBlock(
      headerHash: DoubleSha256Digest,
      confirmations: Int): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash.flip), confirmations)

  def listSinceBlock(
      headerHash: DoubleSha256Digest,
      includeWatchOnly: Boolean): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash.flip), includeWatchOnly = includeWatchOnly)

  def listSinceBlock(
      headerHash: DoubleSha256Digest,
      confirmations: Int,
      includeWatchOnly: Boolean): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash.flip), confirmations, includeWatchOnly)

  def listTransactions(
      account: String = "*",
      count: Int = 10,
      skip: Int = 0,
      includeWatchOnly: Boolean = false): Future[
    Vector[ListTransactionsResult]] = {
    bitcoindCall[Vector[ListTransactionsResult]](
      "listtransactions",
      List(JsString(account),
           JsNumber(count),
           JsNumber(skip),
           JsBoolean(includeWatchOnly)))
  }

  def pruneBlockChain(height: Int): Future[Int] = {
    bitcoindCall[Int]("pruneblockchain", List(JsNumber(height)))
  }

  def rescanBlockChain(): Future[RescanBlockChainResult] =
    rescanBlockChain(None, None)

  private def rescanBlockChain(
      start: Option[Int],
      stop: Option[Int]): Future[RescanBlockChainResult] = {
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
    bitcoindCall[Boolean]("verifychain",
                          List(JsNumber(level), JsNumber(blocks)))
  }
}
