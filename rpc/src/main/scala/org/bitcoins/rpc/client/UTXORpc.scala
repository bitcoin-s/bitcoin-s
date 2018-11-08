package org.bitcoins.rpc.client

import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.BitcoindJsonReaders._
import org.bitcoins.rpc.serializers.BitcoindJsonSerializers._
import play.api.libs.json._

import scala.concurrent.Future

trait UTXORpc extends Client {
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
    addresses: Option[Vector[BitcoinAddress]]): Future[Vector[UnspentOutput]] = {
    val params =
      if (addresses.isEmpty) {
        List(JsNumber(minConfirmations), JsNumber(maxConfirmations))
      } else {
        List(
          JsNumber(minConfirmations),
          JsNumber(maxConfirmations),
          Json.toJson(addresses.get))
      }
    bitcoindCall[Vector[UnspentOutput]]("listunspent", params)
  }

  def lockUnspent(
    unlock: Boolean,
    outputs: Vector[RpcOpts.LockUnspentOutputParameter]): Future[Boolean] = {
    bitcoindCall[Boolean](
      "lockunspent",
      List(JsBoolean(unlock), Json.toJson(outputs)))
  }

}
