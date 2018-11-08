package org.bitcoins.rpc.client

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.jsonmodels.{ GetBlockTemplateResult, GetMiningInfoResult }
import org.bitcoins.rpc.serializers.BitcoindJsonReaders._
import org.bitcoins.rpc.serializers.BitcoindJsonSerializers._
import play.api.libs.json.{ JsNumber, JsString, Json }

import scala.concurrent.Future

/**
 * RPC calls related to mining
 */
trait MiningRpc extends Client {
  def generate(
    blocks: Int,
    maxTries: Int = 1000000): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]](
      "generate",
      List(JsNumber(blocks), JsNumber(maxTries)))
  }

  def generateToAddress(
    blocks: Int,
    address: BitcoinAddress,
    maxTries: Int = 1000000): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]](
      "generatetoaddress",
      List(JsNumber(blocks), JsString(address.toString), JsNumber(maxTries)))
  }

  def getBlockTemplate(
    request: Option[RpcOpts.BlockTemplateRequest] = None): Future[GetBlockTemplateResult] = {
    val params =
      if (request.isEmpty) {
        List.empty
      } else {
        List(Json.toJson(request.get))
      }
    bitcoindCall[GetBlockTemplateResult]("getblocktemplate", params)
  }

  def getNetworkHashPS(
    blocks: Int = 120,
    height: Int = -1): Future[BigDecimal] = {
    bitcoindCall[BigDecimal](
      "getnetworkhashps",
      List(JsNumber(blocks), JsNumber(height)))
  }

  def getMiningInfo: Future[GetMiningInfoResult] = {
    bitcoindCall[GetMiningInfoResult]("getmininginfo")
  }

  def prioritiseTransaction(
    txid: DoubleSha256Digest,
    feeDelta: Satoshis): Future[Boolean] = {
    bitcoindCall[Boolean](
      "prioritisetransaction",
      List(JsString(txid.hex), JsNumber(0), JsNumber(feeDelta.toLong)))

  }
}
