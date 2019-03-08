package org.bitcoins.rpc.client.common

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput}
import org.bitcoins.rpc.jsonmodels.{
  FundRawTransactionResult,
  GetRawTransactionResult,
  RpcTransaction
}
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json._

import scala.concurrent.Future

trait RawTransactionRpc extends Client {

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
    bitcoindCall[RpcTransaction]("decoderawtransaction",
                                 List(JsString(transaction.hex)))
  }

  def fundRawTransaction(
      transaction: Transaction): Future[FundRawTransactionResult] =
    fundRawTransaction(transaction, None)

  private def fundRawTransaction(
      transaction: Transaction,
      options: Option[RpcOpts.FundRawTransactionOptions]): Future[
    FundRawTransactionResult] = {
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
      options: RpcOpts.FundRawTransactionOptions): Future[
    FundRawTransactionResult] = fundRawTransaction(transaction, Some(options))

  def getRawTransaction(
      txid: DoubleSha256Digest,
      blockhash: Option[DoubleSha256Digest] = None): Future[
    GetRawTransactionResult] = {
    bitcoindCall[GetRawTransactionResult](
      "getrawtransaction",
      List(JsString(txid.hex), JsBoolean(true), Json.toJson(blockhash)))
  }

  def getRawTransactionRaw(
      txid: DoubleSha256Digest,
      blockhash: Option[DoubleSha256Digest] = None): Future[Transaction] = {
    bitcoindCall[Transaction](
      "getrawtransaction",
      List(JsString(txid.hex), JsBoolean(false), Json.toJson(blockhash)))
  }

  def sendRawTransaction(
      transaction: Transaction,
      allowHighFees: Boolean = false): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest](
      "sendrawtransaction",
      List(JsString(transaction.hex), JsBoolean(allowHighFees)))
  }
}
