package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind.{
  AnalyzePsbtResult,
  DecodePsbtResult,
  FinalizePsbtResult
}
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput}
import org.bitcoins.core.psbt.PSBT
import play.api.libs.json._

import scala.concurrent.Future

/** Set of utilities to analyze, join, and update existing PSBTs
  *
  * @see [[https://bitcoincore.org/en/doc/0.18.0/rpc/rawtransactions/analyzepsbt/]]
  * @see [[https://bitcoincore.org/en/doc/0.18.0/rpc/rawtransactions/joinpsbts/]]
  * @see [[https://bitcoincore.org/en/doc/0.18.0/rpc/rawtransactions/utxoupdatepsbt/]]
  */
trait PsbtRpc {
  self: Client =>

  def analyzePsbt(psbt: PSBT): Future[AnalyzePsbtResult] = {
    bitcoindCall[AnalyzePsbtResult]("analyzepsbt", List(JsString(psbt.base64)))
  }

  def joinPsbts(psbts: Seq[PSBT]): Future[PSBT] = {
    bitcoindCall[PSBT]("joinpsbts", List(Json.toJson(psbts)))
  }

  def utxoUpdatePsbt(psbt: PSBT): Future[PSBT] = {
    bitcoindCall[PSBT]("utxoupdatepsbt", List(JsString(psbt.base64)))
  }

  def utxoUpdatePsbt(psbt: PSBT, descriptors: Seq[String]): Future[PSBT] = {
    bitcoindCall[PSBT]("utxoupdatepsbt",
                       List(JsString(psbt.base64), Json.toJson(descriptors)))
  }

  def convertToPsbt(
      rawTx: Transaction,
      permitSigData: Boolean = false,
      isWitness: Option[Boolean] = None): Future[PSBT] = {
    val firstArgs: List[JsValue] =
      List(Json.toJson(rawTx), JsBoolean(permitSigData))
    val args: List[JsValue] = firstArgs ++ isWitness.map(Json.toJson(_)).toList
    bitcoindCall[PSBT]("converttopsbt", args)
  }

  def createPsbt(
      inputs: Vector[TransactionInput],
      outputs: Map[BitcoinAddress, CurrencyUnit],
      locktime: Int = 0,
      replacable: Boolean = false): Future[PSBT] = {
    val outputsJson =
      Json.toJson {
        outputs.map { case (addr, curr) =>
          addr -> Bitcoins(curr.satoshis)
        }
      }
    bitcoindCall[PSBT]("createpsbt",
                       List(Json.toJson(inputs),
                            outputsJson,
                            JsNumber(locktime),
                            JsBoolean(replacable)))
  }

  def combinePsbt(psbts: Vector[PSBT]): Future[PSBT] = {
    bitcoindCall[PSBT]("combinepsbt", List(Json.toJson(psbts)))
  }

  def finalizePsbt(
      psbt: PSBT,
      extract: Boolean = true): Future[FinalizePsbtResult] = {
    bitcoindCall[FinalizePsbtResult](
      "finalizepsbt",
      List(JsString(psbt.base64), JsBoolean(extract)))
  }

  def decodePsbt(psbt: PSBT): Future[DecodePsbtResult] =
    bitcoindCall[DecodePsbtResult]("decodepsbt", List(Json.toJson(psbt)))

}
