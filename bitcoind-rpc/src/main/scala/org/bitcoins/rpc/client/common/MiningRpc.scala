package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind.{
  GetBlockTemplateResult,
  GetMiningInfoResult,
  RpcOpts
}
import org.bitcoins.commons.serializers.JsonReaders._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import play.api.libs.json.{JsNumber, JsString, Json}

import scala.concurrent.Future

/** RPC calls related to mining
  */
trait MiningRpc { self: Client =>

  @deprecated("use generateToAddress instead", since = "0.18.0")
  def generate(
      blocks: Int,
      maxTries: Int = 1000000): Future[Vector[DoubleSha256DigestBE]] = {
    bitcoindCall[Vector[DoubleSha256DigestBE]](
      "generate",
      List(JsNumber(blocks), JsNumber(maxTries)))
  }

  def generateToAddress(
      blocks: Int,
      address: BitcoinAddress,
      maxTries: Int = 1000000): Future[Vector[DoubleSha256DigestBE]] = {
    bitcoindCall[Vector[DoubleSha256DigestBE]](
      "generatetoaddress",
      List(JsNumber(blocks), JsString(address.toString), JsNumber(maxTries)))
  }

  def getBlockTemplate(request: Option[RpcOpts.BlockTemplateRequest] =
    None): Future[GetBlockTemplateResult] = {
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
    bitcoindCall[BigDecimal]("getnetworkhashps",
                             List(JsNumber(blocks), JsNumber(height)))
  }

  def getMiningInfo: Future[GetMiningInfoResult] = {
    bitcoindCall[GetMiningInfoResult]("getmininginfo")
  }

  def prioritiseTransaction(
      txid: DoubleSha256DigestBE,
      feeDelta: Satoshis): Future[Boolean] = {
    bitcoindCall[Boolean](
      "prioritisetransaction",
      List(JsString(txid.hex), JsNumber(0), JsNumber(feeDelta.toLong)))
  }

  def prioritiseTransaction(
      txid: DoubleSha256Digest,
      feeDelta: Satoshis): Future[Boolean] = {
    prioritiseTransaction(txid.flip, feeDelta)
  }
}
