package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind.{RpcOpts, UnspentOutput}
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import play.api.libs.json._

import scala.concurrent.Future

/**
  * This trait defines functionality related to
  * UTXOs (unspent transaction outputs).
  *
  * @see [[https://bitcoin.org/en/developer-guide#term-utxo Bitcoin.org]]
  *     developer guide article on UTXOs
  */
trait UTXORpc { self: Client =>

  def listLockUnspent: Future[Vector[TransactionOutPoint]] = {
    bitcoindCall[Vector[TransactionOutPoint]]("listlockunspent")
  }

  def listUnspent: Future[Vector[UnspentOutput]] = listUnspent(addresses = None)

  def listUnspent(
      minConfirmations: Int,
      maxConfirmations: Int): Future[Vector[UnspentOutput]] =
    listUnspent(minConfirmations, maxConfirmations, None)

  def listUnspent(
      addresses: Vector[BitcoinAddress]): Future[Vector[UnspentOutput]] =
    listUnspent(addresses = addresses)

  def listUnspent(
      minConfirmations: Int,
      maxConfirmations: Int,
      addresses: Vector[BitcoinAddress]): Future[Vector[UnspentOutput]] =
    listUnspent(minConfirmations, maxConfirmations, Some(addresses))

  private def listUnspent(
      minConfirmations: Int = 1,
      maxConfirmations: Int = 9999999,
      addresses: Option[Vector[BitcoinAddress]]): Future[
    Vector[UnspentOutput]] = {
    val params =
      List(JsNumber(minConfirmations), JsNumber(maxConfirmations)) ++
        addresses.map(Json.toJson(_)).toList
    bitcoindCall[Vector[UnspentOutput]]("listunspent", params)
  }

  def lockUnspent(
      unlock: Boolean,
      outputs: Vector[RpcOpts.LockUnspentOutputParameter]): Future[Boolean] = {
    bitcoindCall[Boolean]("lockunspent",
                          List(JsBoolean(unlock), Json.toJson(outputs)))
  }

}
