package org.bitcoins.rpc.client.common

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput}
import org.bitcoins.rpc.jsonmodels.{
  FundRawTransactionResult,
  GetRawTransactionResult,
  RpcTransaction,
  SignRawTransactionResult
}
import org.bitcoins.rpc.serializers.JsonSerializers._
import org.bitcoins.rpc.serializers.JsonWriters._
import play.api.libs.json._

import scala.concurrent.Future

/**
  * This trait defines RPC calls relating to interacting
  * with raw transactions. This includes creation, decoding
  * funding and sending.
  */
trait RawTransactionRpc { self: Client =>

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
      txid: DoubleSha256DigestBE,
      blockhash: Option[DoubleSha256DigestBE] = None): Future[
    GetRawTransactionResult] = {
    val lastParam: List[JsString] = blockhash match {
      case Some(hash) => JsString(hash.hex) :: Nil
      case None       => Nil
    }
    val params = List(JsString(txid.hex), JsBoolean(true)) ++ lastParam

    bitcoindCall[GetRawTransactionResult]("getrawtransaction", params)
  }

  def getRawTransactionRaw(
      txid: DoubleSha256DigestBE,
      blockhash: Option[DoubleSha256DigestBE] = None): Future[Transaction] = {
    val lastParam: List[JsString] = blockhash match {
      case Some(hash) => JsString(hash.hex) :: Nil
      case None       => Nil
    }
    val params = List(JsString(txid.hex), JsBoolean(false)) ++ lastParam

    bitcoindCall[Transaction]("getrawtransaction", params)
  }

  def sendRawTransaction(
      transaction: Transaction,
      allowHighFees: Boolean = false): Future[DoubleSha256DigestBE] = {
    bitcoindCall[DoubleSha256DigestBE](
      "sendrawtransaction",
      List(JsString(transaction.hex), JsBoolean(allowHighFees)))
  }

}
