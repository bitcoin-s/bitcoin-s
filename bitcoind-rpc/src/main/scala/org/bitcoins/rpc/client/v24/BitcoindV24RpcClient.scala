package org.bitcoins.rpc.client.v24

import akka.actor.ActorSystem
import org.bitcoins.commons.jsonmodels.bitcoind.{
  GetTxSpendingPrevOutResult,
  SimulateRawTransactionResult
}
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.client.v23.BitcoindV23RpcClient
import org.bitcoins.rpc.config.BitcoindInstance
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

/** Class for creating a BitcoindV24 instance that can access RPCs
  */
class BitcoindV24RpcClient(override val instance: BitcoindInstance)(implicit
    actorSystem: ActorSystem)
    extends BitcoindV23RpcClient(instance) {

  override lazy val version: Future[BitcoindVersion] =
    Future.successful(BitcoindVersion.V24)

  def getTxSpendingPrevOut(
      prevout: TransactionOutPoint): Future[GetTxSpendingPrevOutResult] = {
    getTxSpendingPrevOut(Vector(prevout)).map(_.head)
  }

  def getTxSpendingPrevOut(prevouts: Vector[TransactionOutPoint]): Future[
    Vector[GetTxSpendingPrevOutResult]] = {
    val json = JsArray(prevouts.map { prev =>
      Json.obj("txid" -> prev.txIdBE.hex, "vout" -> prev.vout.toLong)
    })

    bitcoindCall[Vector[GetTxSpendingPrevOutResult]]("gettxspendingprevout",
                                                     List(json))
  }

  def simulateRawTransaction(
      tx: Transaction,
      includeWatchOnly: Boolean = true): Future[CurrencyUnit] = {
    simulateRawTransactions(Vector(tx), includeWatchOnly)
  }

  def simulateRawTransactions(
      txs: Vector[Transaction],
      includeWatchOnly: Boolean = true): Future[CurrencyUnit] = {
    val txsJson = JsArray(txs.map(tx => JsString(tx.hex)))
    val options = Json.obj("include_watchonly" -> includeWatchOnly)

    bitcoindCall[SimulateRawTransactionResult](
      "simulaterawtransaction",
      List(txsJson, options)).map(_.balance_change)
  }
}

object BitcoindV24RpcClient {

  /** Creates an RPC client from the given instance.
    *
    * Behind the scenes, we create an actor system for
    * you. You can use `withActorSystem` if you want to
    * manually specify an actor system for the RPC client.
    */
  def apply(instance: BitcoindInstance): BitcoindV24RpcClient = {
    implicit val system: ActorSystem =
      ActorSystem.create(BitcoindRpcClient.ActorSystemName)
    withActorSystem(instance)
  }

  /** Creates an RPC client from the given instance,
    * together with the given actor system. This is for
    * advanced users, where you need fine grained control
    * over the RPC client.
    */
  def withActorSystem(instance: BitcoindInstance)(implicit
      system: ActorSystem): BitcoindV24RpcClient =
    new BitcoindV24RpcClient(instance)(system)

  def fromUnknownVersion(
      rpcClient: BitcoindRpcClient): Try[BitcoindV24RpcClient] =
    Try {
      new BitcoindV24RpcClient(rpcClient.instance)(rpcClient.system)
    }

}
