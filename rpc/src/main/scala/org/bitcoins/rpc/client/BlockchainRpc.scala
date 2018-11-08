package org.bitcoins.rpc.client

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.{ Block, BlockHeader }
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.BitcoindJsonReaders._
import org.bitcoins.rpc.serializers.BitcoindJsonSerializers._
import play.api.libs.json.{ JsBoolean, JsNumber, JsString }

import scala.concurrent.Future

/**
 * RPC calls related to querying the state of the blockchain
 */
trait BlockchainRpc extends Client {
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

  def getChainTxStats: Future[GetChainTxStatsResult] =
    getChainTxStats(None, None)

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

  def getChainTxStats(blocks: Int): Future[GetChainTxStatsResult] =
    getChainTxStats(Some(blocks), None)

  def getChainTxStats(
    blocks: Int,
    blockHash: DoubleSha256Digest): Future[GetChainTxStatsResult] =
    getChainTxStats(Some(blocks), Some(blockHash))

  def getDifficulty: Future[BigDecimal] = {
    bitcoindCall[BigDecimal]("getdifficulty")
  }

  def invalidateBlock(blockHash: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("invalidateblock", List(JsString(blockHash.hex)))
  }

  def listSinceBlock: Future[ListSinceBlockResult] = listSinceBlock(None)

  def listSinceBlock(
    headerHash: Option[DoubleSha256Digest] = None,
    confirmations: Int = 1,
    includeWatchOnly: Boolean = false): Future[ListSinceBlockResult] = {
    val params =
      if (headerHash.isEmpty) {
        List.empty
      } else {
        List(
          JsString(headerHash.get.hex),
          JsNumber(confirmations),
          JsBoolean(includeWatchOnly))
      }
    bitcoindCall[ListSinceBlockResult]("listsinceblock", params)
  }

  def listSinceBlock(
    headerHash: DoubleSha256Digest): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash))

  def listSinceBlock(
    headerHash: DoubleSha256Digest,
    confirmations: Int): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash), confirmations)

  def listSinceBlock(
    headerHash: DoubleSha256Digest,
    includeWatchOnly: Boolean): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash), includeWatchOnly = includeWatchOnly)

  def listSinceBlock(
    headerHash: DoubleSha256Digest,
    confirmations: Int,
    includeWatchOnly: Boolean): Future[ListSinceBlockResult] =
    listSinceBlock(Some(headerHash), confirmations, includeWatchOnly)

  def listTransactions(
    account: String = "*",
    count: Int = 10,
    skip: Int = 0,
    includeWatchOnly: Boolean = false): Future[Vector[ListTransactionsResult]] = {
    bitcoindCall[Vector[ListTransactionsResult]](
      "listtransactions",
      List(
        JsString(account),
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

  def preciousBlock(headerHash: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("preciousblock", List(JsString(headerHash.hex)))
  }

  def validateAddress(
    address: BitcoinAddress): Future[ValidateAddressResult] = {
    bitcoindCall[ValidateAddressResult](
      "validateaddress",
      List(JsString(address.toString)))
  }

  def verifyChain(level: Int = 3, blocks: Int = 6): Future[Boolean] = {
    bitcoindCall[Boolean](
      "verifychain",
      List(JsNumber(level), JsNumber(blocks)))
  }
}
