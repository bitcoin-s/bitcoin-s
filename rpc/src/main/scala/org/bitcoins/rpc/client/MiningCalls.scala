package org.bitcoins.rpc.client

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.jsonmodels.{GetBlockTemplateResult, GetMiningInfoResult}
import org.bitcoins.rpc.serializers.JsonReaders._
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json.{JsNumber, JsString, Json}

import scala.concurrent.Future

/**
  * RPC calls related to mining
  */
protected trait MiningCalls extends Client with BitcoindCall {
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

  def getMiningInfo: Future[GetMiningInfoResult] = {
    bitcoindCall[GetMiningInfoResult]("getmininginfo")
  }
}
