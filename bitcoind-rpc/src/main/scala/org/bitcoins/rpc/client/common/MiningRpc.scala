package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind.{
  GenerateBlockResult,
  GetBlockTemplateResult,
  GetMiningInfoResult,
  GetMiningInfoResultV28,
  GetMiningInfoResultV29,
  GetMiningInfoResultV30,
  PrioritisedTransaction,
  RpcOpts
}
import org.bitcoins.commons.serializers.JsonReaders.*
import org.bitcoins.commons.serializers.JsonSerializers.*
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.rpc.client.common.BitcoindVersion.{Unknown, V28, V29, V30}
import play.api.libs.json.{JsArray, JsNumber, JsString, Json}

import scala.concurrent.Future

/** RPC calls related to mining
  */
trait MiningRpc { self: Client & BlockchainRpc =>

  def generateToAddress(
      blocks: Int,
      address: BitcoinAddress,
      maxTries: Int = 1000000
  ): Future[Vector[DoubleSha256DigestBE]] = {
    val hashesF = bitcoindCall[Vector[DoubleSha256DigestBE]](
      "generatetoaddress",
      List(JsNumber(blocks), JsString(address.toString), JsNumber(maxTries))
    )

    for {
      hashes <- hashesF
      _ <- syncWithValidationInterfaceQueue()
    } yield hashes
  }

  def generateBlock(
      address: BitcoinAddress,
      transactions: Vector[Transaction]
  ): Future[DoubleSha256DigestBE] = {
    val txsJs = JsArray(transactions.map(t => JsString(t.hex)))
    val hashesF = bitcoindCall[GenerateBlockResult](
      "generateblock",
      List(JsString(address.toString), txsJs)
    ).map(_.hash)
    for {
      hash <- hashesF
      _ <- syncWithValidationInterfaceQueue()
    } yield hash
  }

  def getBlockTemplate(
      request: Option[RpcOpts.BlockTemplateRequest] = None
  ): Future[GetBlockTemplateResult] = {
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
      height: Int = -1
  ): Future[BigDecimal] = {
    bitcoindCall[BigDecimal](
      "getnetworkhashps",
      List(JsNumber(blocks), JsNumber(height))
    )
  }

  def getMiningInfo: Future[GetMiningInfoResult] = {
    self.version.flatMap {
      case V28 | Unknown =>
        bitcoindCall[GetMiningInfoResultV28]("getmininginfo")
      case V29 =>
        bitcoindCall[GetMiningInfoResultV29]("getmininginfo")
      case V30 => bitcoindCall[GetMiningInfoResultV30]("getmininginfo")
    }
  }

  def prioritiseTransaction(
      txid: DoubleSha256DigestBE,
      feeDelta: Satoshis
  ): Future[Boolean] = {
    bitcoindCall[Boolean](
      "prioritisetransaction",
      List(JsString(txid.hex), JsNumber(0), JsNumber(feeDelta.toLong))
    )
  }

  def prioritiseTransaction(
      txid: DoubleSha256Digest,
      feeDelta: Satoshis
  ): Future[Boolean] = {
    prioritiseTransaction(txid.flip, feeDelta)
  }

  def getPrioritisedTransactions()
      : Future[Map[DoubleSha256DigestBE, PrioritisedTransaction]] = {
    bitcoindCall[Map[DoubleSha256DigestBE, PrioritisedTransaction]](
      "getprioritisedtransactions")
  }

  def generateToDescriptor(
      numBlocks: Int,
      descriptor: String,
      maxTries: Long = 1000000
  ): Future[Vector[DoubleSha256DigestBE]] = {
    bitcoindCall[Vector[DoubleSha256DigestBE]](
      "generatetodescriptor",
      List(JsNumber(numBlocks), JsString(descriptor), JsNumber(maxTries))
    )
  }
}
