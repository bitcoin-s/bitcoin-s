package org.bitcoins.rpc.client.v16

import akka.actor.ActorSystem
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.rpc.client.common.{
  BitcoindRpcClient,
  BitcoindVersion,
  RpcOpts
}
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.jsonmodels.SignRawTransactionResult
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json.{JsArray, JsString, Json}

import scala.concurrent.Future
import scala.util.Try

/**
  * This class is compatible with version 0.16 of Bitcoin Core.
  */
class BitcoindV16RpcClient(override val instance: BitcoindInstance)(
    implicit
    actorSystem: ActorSystem)
    extends BitcoindRpcClient(instance)
    with V16AccountRpc
    with V16SendRpc {

  override def version: BitcoindVersion = BitcoindVersion.V16

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
      utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter]): Future[
    SignRawTransactionResult] =
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

object BitcoindV16RpcClient {

  def fromUnknownVersion(rpcClient: BitcoindRpcClient)(
      implicit actorSystem: ActorSystem): Try[BitcoindV16RpcClient] =
    Try {
      new BitcoindV16RpcClient(rpcClient.instance)
    }
}
