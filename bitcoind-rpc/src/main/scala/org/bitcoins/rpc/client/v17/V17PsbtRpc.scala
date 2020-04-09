package org.bitcoins.rpc.client.v17

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.WalletCreateFundedPsbtOptions
import org.bitcoins.commons.jsonmodels.bitcoind.{
  DecodePsbtResult,
  FinalizePsbtResult,
  WalletCreateFundedPsbtResult,
  WalletProcessPsbtResult
}
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.rpc.client.common.Client
import play.api.libs.json._

import scala.concurrent.Future

/**
  * RPC calls related to PSBT (partially signed bitcoin transactions)
  * handling in Bitcoin Core.
  *
  * @note The PSBT format is currently not supported by Bitcoin-S.
  *       Therefore raw strings are returned in several of these
  *       RPCs.
  *
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki BIP174]] on PSBTs
  *     and  [[https://github.com/bitcoin/bitcoin/blob/master/doc/psbt.md PSBT Howto for Bitcoin Core]]
  */
trait V17PsbtRpc { self: Client =>

  def convertToPsbt(
      rawTx: Transaction,
      permitSigData: Boolean = false,
      isWitness: Option[Boolean] = None): Future[String] = {
    val firstArgs: List[JsValue] =
      List(Json.toJson(rawTx), JsBoolean(permitSigData))
    val args: List[JsValue] = firstArgs ++ isWitness.map(Json.toJson(_)).toList
    bitcoindCall[String]("converttopsbt", args)
  }

  def createPsbt(
      inputs: Vector[TransactionInput],
      outputs: Map[BitcoinAddress, CurrencyUnit],
      locktime: Int = 0,
      replacable: Boolean = false): Future[String] = {
    val outputsJson =
      Json.toJson {
        outputs.map {
          case (addr, curr) => addr -> Bitcoins(curr.satoshis)
        }
      }
    bitcoindCall[String]("createpsbt",
                         List(Json.toJson(inputs),
                              outputsJson,
                              JsNumber(locktime),
                              JsBoolean(replacable)))
  }

  def combinePsbt(psbts: Vector[String]): Future[String] = {
    bitcoindCall[String]("combinepsbt", List(Json.toJson(psbts)))
  }

  def finalizePsbt(
      psbt: String,
      extract: Boolean = true): Future[FinalizePsbtResult] = {
    bitcoindCall[FinalizePsbtResult]("finalizepsbt",
                                     List(JsString(psbt), JsBoolean(extract)))
  }

  def walletCreateFundedPsbt(
      inputs: Vector[TransactionInput],
      outputs: Map[BitcoinAddress, CurrencyUnit],
      locktime: Int = 0,
      options: WalletCreateFundedPsbtOptions = WalletCreateFundedPsbtOptions(),
      bip32derivs: Boolean = false
  ): Future[WalletCreateFundedPsbtResult] = {
    val jsonOutputs =
      Json.toJson {
        outputs.map { case (addr, curr) => addr -> Bitcoins(curr.satoshis) }
      }
    bitcoindCall[WalletCreateFundedPsbtResult](
      "walletcreatefundedpsbt",
      List(Json.toJson(inputs),
           jsonOutputs,
           JsNumber(locktime),
           Json.toJson(options),
           Json.toJson(bip32derivs))
    )
  }

  def walletProcessPsbt(
      psbt: String,
      sign: Boolean = true,
      sighashType: HashType = HashType.sigHashAll,
      bip32Derivs: Boolean = false
  ): Future[WalletProcessPsbtResult] = {
    val args = List(JsString(psbt),
                    JsBoolean(sign),
                    Json.toJson(sighashType),
                    JsBoolean(bip32Derivs))

    bitcoindCall[WalletProcessPsbtResult]("walletprocesspsbt", args)
  }

  def decodePsbt(psbt: String): Future[DecodePsbtResult] =
    bitcoindCall[DecodePsbtResult]("decodepsbt", List(Json.toJson(psbt)))
}
