package org.bitcoins.rpc.client.common

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.{ Bitcoins, Satoshis }
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.util.BitcoinSUtil
import org.bitcoins.rpc.client.common.RpcOpts.AddressType
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.BitcoindJsonSerializers._
import play.api.libs.json._

import scala.concurrent.Future

trait TransactionRpc extends Client {

  def abandonTransaction(txid: DoubleSha256Digest): Future[Unit] = {
    bitcoindCall[Unit]("abandontransaction", List(JsString(txid.hex)))
  }

  def bumpFee(
    txid: DoubleSha256Digest,
    confTarget: Int = 6,
    totalFee: Option[Satoshis] = None,
    replaceable: Boolean = true,
    estimateMode: String = "UNSET"): Future[BumpFeeResult] = {
    val options =
      if (totalFee.isEmpty) {
        List(
          ("confTarget", JsNumber(confTarget)),
          ("replaceable", JsBoolean(replaceable)),
          ("estimate_mode", JsString(estimateMode)))
      } else {
        List(
          ("confTarget", JsNumber(confTarget)),
          ("totalFee", JsNumber(totalFee.get.toBigDecimal)),
          ("replaceable", JsBoolean(replaceable)),
          ("estimate_mode", JsString(estimateMode)))
      }

    bitcoindCall[BumpFeeResult](
      "bumpfee",
      List(JsString(txid.hex), JsObject(options)))
  }

  // TODO: add ScriptPubKey.asmHex
  def decodeScript(script: ScriptPubKey): Future[DecodeScriptResult] = {
    bitcoindCall[DecodeScriptResult](
      "decodescript",
      List(JsString(BitcoinSUtil.encodeHex(script.asmBytes))))
  }

  // Needs manual testing!
  def estimateSmartFee(
    blocks: Int,
    mode: String = "CONSERVATIVE"): Future[EstimateSmartFeeResult] = {
    bitcoindCall[EstimateSmartFeeResult](
      "estimatesmartfee",
      List(JsNumber(blocks), JsString(mode)))
  }

  def getTransaction(
    txid: DoubleSha256Digest,
    watchOnly: Boolean = false): Future[GetTransactionResult] = {
    bitcoindCall[GetTransactionResult](
      "gettransaction",
      List(JsString(txid.hex), JsBoolean(watchOnly)))
  }

  def getTxOut(
    txid: DoubleSha256Digest,
    vout: Int,
    includeMemPool: Boolean = true): Future[GetTxOutResult] = {
    bitcoindCall[GetTxOutResult](
      "gettxout",
      List(JsString(txid.hex), JsNumber(vout), JsBoolean(includeMemPool)))
  }

  def getTxOutProof(txids: Vector[DoubleSha256Digest]): Future[MerkleBlock] =
    getTxOutProof(txids, None)

  private def getTxOutProof(
    txids: Vector[DoubleSha256Digest],
    headerHash: Option[DoubleSha256Digest]): Future[MerkleBlock] = {
    val params = {
      val hashes = JsArray(txids.map(hash => JsString(hash.hex)))
      if (headerHash.isEmpty) {
        List(hashes)
      } else {
        List(hashes, JsString(headerHash.get.hex))
      }
    }
    bitcoindCall[MerkleBlock]("gettxoutproof", params)
  }

  def getTxOutProof(
    txids: Vector[DoubleSha256Digest],
    headerHash: DoubleSha256Digest): Future[MerkleBlock] =
    getTxOutProof(txids, Some(headerHash))

  def verifyTxOutProof(
    proof: MerkleBlock): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]](
      "verifytxoutproof",
      List(JsString(proof.hex)))
  }

  def getTxOutSetInfo: Future[GetTxOutSetInfoResult] = {
    bitcoindCall[GetTxOutSetInfoResult]("gettxoutsetinfo")
  }

  def getRawChangeAddress: Future[BitcoinAddress] = getRawChangeAddress(None)

  def getRawChangeAddress(addressType: AddressType): Future[BitcoinAddress] =
    getRawChangeAddress(Some(addressType))

  private def getRawChangeAddress(
    addressType: Option[AddressType]): Future[BitcoinAddress] = {
    if (addressType.isEmpty) {
      bitcoindCall[BitcoinAddress]("getrawchangeaddress")
    } else {
      bitcoindCall[BitcoinAddress](
        "getrawchangeaddress",
        List(JsString(addressType
          .map(_.toString)
          .getOrElse(""))))
    }
  }

  def sendMany(
    amounts: Map[BitcoinAddress, Bitcoins],
    minconf: Int = 1,
    comment: String = "",
    subtractFeeFrom: Vector[BitcoinAddress] = Vector.empty): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest](
      "sendmany",
      List(
        JsString(""),
        Json.toJson(amounts),
        JsNumber(minconf),
        JsString(comment),
        Json.toJson(subtractFeeFrom)))
  }

  def sendToAddress(
    address: BitcoinAddress,
    amount: Bitcoins,
    localComment: String = "",
    toComment: String = "",
    subractFeeFromAmount: Boolean = false): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest](
      "sendtoaddress",
      List(
        JsString(address.toString),
        JsNumber(amount.toBigDecimal),
        JsString(localComment),
        JsString(toComment),
        JsBoolean(subractFeeFromAmount)))
  }
}
