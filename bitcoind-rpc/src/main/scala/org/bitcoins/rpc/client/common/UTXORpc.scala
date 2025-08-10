package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.DumpTxOutSetType
import org.bitcoins.commons.jsonmodels.bitcoind.{
  DumpTxOutSetResult,
  LoadTxOutSetResult,
  RpcOpts,
  ScanTxoutSetRequest,
  ScanTxoutSetResult,
  UnspentOutput
}
import org.bitcoins.commons.serializers.JsonSerializers
import org.bitcoins.commons.serializers.JsonSerializers.*
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import play.api.libs.json.*

import java.nio.file.Path
import scala.concurrent.Future

/** This trait defines functionality related to UTXOs (unspent transaction
  * outputs).
  *
  * @see
  *   [[https://bitcoin.org/en/developer-guide#term-utxo Bitcoin.org]] developer
  *   guide article on UTXOs
  */
trait UTXORpc { self: Client =>

  def listLockUnspent: Future[Vector[TransactionOutPoint]] = {
    bitcoindCall[Vector[TransactionOutPoint]]("listlockunspent")
  }

  def listUnspent: Future[Vector[UnspentOutput]] = listUnspent(addresses = None)

  def listUnspent(walletName: String): Future[Vector[UnspentOutput]] =
    listUnspent(addresses = None, walletNameOpt = Some(walletName))

  def listUnspent(
      minConfirmations: Int,
      maxConfirmations: Int
  ): Future[Vector[UnspentOutput]] =
    listUnspent(minConfirmations, maxConfirmations, None)

  def listUnspent(
      addresses: Vector[BitcoinAddress]
  ): Future[Vector[UnspentOutput]] =
    listUnspent(addresses = Some(addresses))

  def listUnspent(
      minConfirmations: Int,
      maxConfirmations: Int,
      addresses: Vector[BitcoinAddress]
  ): Future[Vector[UnspentOutput]] =
    listUnspent(minConfirmations, maxConfirmations, Some(addresses))

  private def listUnspent(
      minConfirmations: Int = 1,
      maxConfirmations: Int = 9999999,
      addresses: Option[Vector[BitcoinAddress]],
      walletNameOpt: Option[String] = None
  ): Future[Vector[UnspentOutput]] = {
    val params =
      List(JsNumber(minConfirmations), JsNumber(maxConfirmations)) ++
        addresses.map(Json.toJson(_)).toList
    bitcoindCall[Vector[UnspentOutput]](
      "listunspent",
      params,
      uriExtensionOpt = walletNameOpt.map(walletExtension)
    )
  }

  def lockUnspent(
      unlock: Boolean,
      outputs: Vector[RpcOpts.LockUnspentOutputParameter]
  ): Future[Boolean] = {
    bitcoindCall[Boolean](
      "lockunspent",
      List(JsBoolean(unlock), Json.toJson(outputs))
    )
  }

  def dumpTxOutSet(
      path: Path,
      dumpTxOutSetType: DumpTxOutSetType): Future[DumpTxOutSetResult] = {
    bitcoindCall[DumpTxOutSetResult](
      "dumptxoutset",
      List(Json.toJson(path.toString), dumpTxOutSetType.toJson)
    )
  }

  def loadTxOutSet(path: Path): Future[LoadTxOutSetResult] = {
    bitcoindCall[LoadTxOutSetResult](
      "loadtxoutset",
      List(Json.toJson(path.toString))
    )(JsonSerializers.loadTxOutSetResultReads)
  }

  def scanTxoutSet(request: ScanTxoutSetRequest): Future[ScanTxoutSetResult] = {
    bitcoindCall[ScanTxoutSetResult]("scantxoutset", request.params)(
      JsonSerializers.scanTxoutSetResultReads)
  }

}
