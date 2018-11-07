package org.bitcoins.rpc.client

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonReaders._
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json.{JsBoolean, JsNumber, JsString}

import scala.concurrent.Future

/**
  * RPC calls related to querying the state of the blockchain
  */
protected trait BlockchainRpc extends Client with BitcoindCall {
  def getBestBlockHash: Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest]("getbestblockhash")
  }

  def getBlock(headerHash: DoubleSha256Digest): Future[GetBlockResult] = {
    val isJsonObject = JsNumber(1)
    bitcoindCall[GetBlockResult](
      "getblock",
      List(JsString(headerHash.hex), isJsonObject))
  }

  def getBlockChainInfo: Future[GetBlockChainInfoResult] = {
    bitcoindCall[GetBlockChainInfoResult]("getblockchaininfo")
  }

  def getBlockCount: Future[Int] = {
    bitcoindCall[Int]("getblockcount")
  }

  def getBlockHash(height: Int): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest]("getblockhash", List(JsNumber(height)))
  }

  def getBlockHeader(
                      headerHash: DoubleSha256Digest): Future[GetBlockHeaderResult] = {
    bitcoindCall[GetBlockHeaderResult](
      "getblockheader",
      List(JsString(headerHash.hex), JsBoolean(true)))
  }


  def getBlockHeaderRaw(headerHash: DoubleSha256Digest): Future[BlockHeader] = {
    bitcoindCall[BlockHeader](
      "getblockheader",
      List(JsString(headerHash.hex), JsBoolean(false)))
  }

  def getBlockRaw(headerHash: DoubleSha256Digest): Future[Block] = {
    bitcoindCall[Block]("getblock", List(JsString(headerHash.hex), JsNumber(0)))
  }

  def getBlockWithTransactions(headerHash: DoubleSha256Digest): Future[GetBlockWithTransactionsResult] = {
    val isVerboseJsonObject = JsNumber(2)
    bitcoindCall[GetBlockWithTransactionsResult](
      "getblock",
      List(JsString(headerHash.hex), isVerboseJsonObject))
  }

  def getChainTips: Future[Vector[ChainTip]] = {
    bitcoindCall[Vector[ChainTip]]("getchaintips")
  }

  private def getChainTxStats(
    blocks: Option[Int],
    blockHash: Option[DoubleSha256Digest]): Future[GetChainTxStatsResult] = {
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

  def getChainTxStats: Future[GetChainTxStatsResult] =
    getChainTxStats(None, None)

  def getChainTxStats(blocks: Int): Future[GetChainTxStatsResult] =
    getChainTxStats(Some(blocks), None)

  def getChainTxStats(
    blocks: Int,
    blockHash: DoubleSha256Digest): Future[GetChainTxStatsResult] =
    getChainTxStats(Some(blocks), Some(blockHash))


  def getDifficulty: Future[BigDecimal] = {
    bitcoindCall[BigDecimal]("getdifficulty")
  }
}
