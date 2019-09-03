package org.bitcoins.rpc.client.v16

import akka.actor.ActorSystem
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.rpc.client.common.{
  BitcoindRpcClient,
  BitcoindVersion,
  RpcOpts
}
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.jsonmodels.SignRawTransactionResult
import org.bitcoins.rpc.serializers.JsonSerializers._
import org.bitcoins.rpc.serializers.JsonWriters._
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

/**
  * This class is compatible with version 0.16 of Bitcoin Core.
  *
  * @see [[org.bitcoins.rpc.client.common.BitcoindRpcClient BitcoindRpcClient Scaladocs]]
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
      sigHash: Option[HashType]): Future[SignRawTransactionResult] = {

    val utxos: JsValue = utxoDeps.map(Json.toJson(_)).getOrElse(JsNull)
    val jsonKeys: JsValue = keys.map(Json.toJson(_)).getOrElse(JsNull)

    val params =
      List(JsString(transaction.hex),
           utxos,
           jsonKeys,
           Json.toJson(sigHash.getOrElse(HashType.sigHashAll)))

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
      sigHash: HashType): Future[SignRawTransactionResult] =
    signRawTransaction(transaction, Some(utxoDeps), Some(keys), Some(sigHash))

}

object BitcoindV16RpcClient {

  /**
    * Creates an RPC client from the given instance.
    *
    * Behind the scenes, we create an actor system for
    * you. You can use `withActorSystem` if you want to
    * manually specify an actor system for the RPC client.
    */
  def apply(instance: BitcoindInstance): BitcoindV16RpcClient = {
    implicit val system = ActorSystem.create(BitcoindRpcClient.ActorSystemName)
    withActorSystem(instance)
  }

  /**
    * Creates an RPC client from the given instance,
    * together with the given actor system. This is for
    * advanced users, wher you need fine grained control
    * over the RPC client.
    */
  def withActorSystem(instance: BitcoindInstance)(
      implicit system: ActorSystem): BitcoindV16RpcClient =
    new BitcoindV16RpcClient(instance)

  def fromUnknownVersion(
      rpcClient: BitcoindRpcClient): Try[BitcoindV16RpcClient] =
    Try {
      new BitcoindV16RpcClient(rpcClient.instance)(rpcClient.system)
    }
}
