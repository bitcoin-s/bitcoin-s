package org.bitcoins.rpc.client

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.{ DoubleSha256Digest, ECPrivateKey }
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.{ Transaction, TransactionInput }
import org.bitcoins.rpc.jsonmodels.{ FundRawTransactionResult, GetRawTransactionResult, RpcTransaction, SignRawTransactionResult }
import org.bitcoins.rpc.serializers.BitcoindJsonReaders._
import org.bitcoins.rpc.serializers.BitcoindJsonSerializers._
import org.bitcoins.rpc.serializers.BitcoindJsonWriters._
import play.api.libs.json._

import scala.concurrent.Future

protected trait RawTransactionRpc extends Client {

  def combineRawTransaction(txs: Vector[Transaction]): Future[Transaction] = {
    bitcoindCall[Transaction]("combinerawtransaction", List(Json.toJson(txs)))
  }

  def createRawTransaction(
    inputs: Vector[TransactionInput],
    outputs: Map[BitcoinAddress, Bitcoins],
    locktime: Int = 0): Future[Transaction] = {
    bitcoindCall[Transaction](
      "createrawtransaction",
      List(Json.toJson(inputs), Json.toJson(outputs), JsNumber(locktime)))
  }

  def decodeRawTransaction(transaction: Transaction): Future[RpcTransaction] = {
    bitcoindCall[RpcTransaction](
      "decoderawtransaction",
      List(JsString(transaction.hex)))
  }

  def fundRawTransaction(
    transaction: Transaction): Future[FundRawTransactionResult] =
    fundRawTransaction(transaction, None)

  private def fundRawTransaction(
    transaction: Transaction,
    options: Option[RpcOpts.FundRawTransactionOptions]): Future[FundRawTransactionResult] = {
    val params =
      if (options.isEmpty) {
        List(JsString(transaction.hex))
      } else {
        List(JsString(transaction.hex), Json.toJson(options.get))
      }

    bitcoindCall[FundRawTransactionResult]("fundrawtransaction", params)
  }

  def fundRawTransaction(
    transaction: Transaction,
    options: RpcOpts.FundRawTransactionOptions): Future[FundRawTransactionResult] = fundRawTransaction(transaction, Some(options))

  def getRawTransaction(
    txid: DoubleSha256Digest): Future[GetRawTransactionResult] = {
    bitcoindCall[GetRawTransactionResult](
      "getrawtransaction",
      List(JsString(txid.hex), JsBoolean(true)))
  }

  def getRawTransactionRaw(txid: DoubleSha256Digest): Future[Transaction] = {
    bitcoindCall[Transaction](
      "getrawtransaction",
      List(JsString(txid.hex), JsBoolean(false)))
  }

  def sendRawTransaction(
    transaction: Transaction,
    allowHighFees: Boolean = false): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest](
      "sendrawtransaction",
      List(JsString(transaction.hex), JsBoolean(allowHighFees)))
  }

  def signRawTransaction(
    transaction: Transaction): Future[SignRawTransactionResult] =
    signRawTransaction(transaction, None, None, None)

  private def signRawTransaction(
    transaction: Transaction,
    utxoDeps: Option[Vector[RpcOpts.SignRawTransactionOutputParameter]],
    keys: Option[Vector[ECPrivateKey]],
    sigHash: Option[String]): Future[SignRawTransactionResult] = {

    val utxos = utxoDeps.map(Json.toJson(_)).getOrElse(JsArray.empty)
    val jsonKeys = keys.map(Json.toJson(_)).getOrElse(JsArray.empty)

    val params =
      if (utxoDeps.isEmpty) {
        List(JsString(transaction.hex))
      } else if (keys.isEmpty) {
        List(JsString(transaction.hex), utxos)
      } else if (sigHash.isEmpty) {
        List(JsString(transaction.hex), utxos, jsonKeys)
      } else {
        List(JsString(transaction.hex), utxos, jsonKeys, JsString(sigHash.get))
      }

    bitcoindCall[SignRawTransactionResult]("signrawtransaction", params)
  }

  def signRawTransaction(
    transaction: Transaction,
    utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter]): Future[SignRawTransactionResult] =
    signRawTransaction(transaction, Some(utxoDeps), None, None)

  def signRawTransaction(
    transaction: Transaction,
    utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter],
    keys: Vector[ECPrivateKey]): Future[SignRawTransactionResult] =
    signRawTransaction(transaction, Some(utxoDeps), Some(keys), None)

  def signRawTransaction(
    transaction: Transaction,
    utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter],
    keys: Vector[ECPrivateKey],
    sigHash: String): Future[SignRawTransactionResult] =
    signRawTransaction(transaction, Some(utxoDeps), Some(keys), Some(sigHash))
}
