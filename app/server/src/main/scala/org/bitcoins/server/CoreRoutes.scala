package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import org.bitcoins.commons.jsonmodels.{SerializedPSBT, SerializedTransaction}
import org.bitcoins.core.api.core.CoreApi
import org.bitcoins.core.hd.AddressType
import org.bitcoins.core.protocol.{Bech32Address, P2SHAddress}
import org.bitcoins.core.protocol.script.{
  MultiSignatureScriptPubKey,
  P2SHScriptPubKey,
  P2WSHWitnessSPKV0
}
import org.bitcoins.server.BitcoinSAppConfig.toChainConf
import org.bitcoins.server.routes.{Server, ServerCommand, ServerRoute}
import ujson._

import scala.collection.mutable

case class CoreRoutes(core: CoreApi)(implicit
    system: ActorSystem,
    config: BitcoinSAppConfig)
    extends ServerRoute {
  import system.dispatcher

  def handleCommand: PartialFunction[ServerCommand, Route] = {
    case ServerCommand("finalizepsbt", arr) =>
      withValidServerCommand(FinalizePSBT.fromJsArr(arr)) {
        case FinalizePSBT(psbt) =>
          complete {
            core
              .finalizePSBT(psbt)
              .map(finalized => Server.httpSuccess(finalized.base64))
          }
      }

    case ServerCommand("extractfrompsbt", arr) =>
      withValidServerCommand(ExtractFromPSBT.fromJsArr(arr)) {
        case ExtractFromPSBT(psbt) =>
          complete {
            core
              .extractFromPSBT(psbt)
              .map(tx => Server.httpSuccess(tx.hex))
          }
      }

    case ServerCommand("converttopsbt", arr) =>
      withValidServerCommand(ConvertToPSBT.fromJsArr(arr)) {
        case ConvertToPSBT(tx) =>
          complete {
            core
              .convertToPSBT(tx)
              .map(psbt => Server.httpSuccess(psbt.base64))
          }
      }

    case ServerCommand("combinepsbts", arr) =>
      withValidServerCommand(CombinePSBTs.fromJsArr(arr)) {
        case CombinePSBTs(psbts) =>
          complete {
            core
              .combinePSBTs(psbts)
              .map(psbt => Server.httpSuccess(psbt.base64))
          }
      }

    case ServerCommand("joinpsbts", arr) =>
      withValidServerCommand(JoinPSBTs.fromJsArr(arr)) {
        case JoinPSBTs(psbts) =>
          complete {
            core
              .joinPSBTs(psbts)
              .map(psbt => Server.httpSuccess(psbt.base64))
          }
      }

    case ServerCommand("decoderawtransaction", arr) =>
      withValidServerCommand(DecodeRawTransaction.fromJsArr(arr)) {
        case DecodeRawTransaction(tx) =>
          complete {
            val decoded = SerializedTransaction.decodeRawTransaction(tx)
            val uJson = ujson.read(decoded.toJson.toString())
            Server.httpSuccess(uJson)
          }
      }

    case ServerCommand("decodepsbt", arr) =>
      withValidServerCommand(DecodePSBT.fromJsArr(arr)) {
        case DecodePSBT(psbt) =>
          complete {
            val decoded = SerializedPSBT.decodePSBT(psbt)
            val uJson = ujson.read(decoded.toJson.toString())
            Server.httpSuccess(uJson)
          }
      }

    case ServerCommand("analyzepsbt", arr) =>
      withValidServerCommand(AnalyzePSBT.fromJsArr(arr)) {
        case AnalyzePSBT(psbt) =>
          complete {
            val inputs = psbt.inputMaps.zipWithIndex.map {
              case (inputMap, index) =>
                val txIn = psbt.transaction.inputs(index)
                val vout = txIn.previousOutput.vout.toInt
                val nextRole = inputMap.nextRole(txIn)
                val hasUtxo = inputMap.prevOutOpt(vout).isDefined
                val isFinalized = inputMap.isFinalized
                val missingSigs = inputMap.missingSignatures(vout)

                if (missingSigs.isEmpty) {
                  Obj(
                    "has_utxo" -> Bool(hasUtxo),
                    "is_final" -> Bool(isFinalized),
                    "next" -> Str(nextRole.shortName)
                  )
                } else {
                  Obj(
                    "has_utxo" -> Bool(hasUtxo),
                    "is_final" -> Bool(isFinalized),
                    "missing_sigs" -> missingSigs.map(hash => Str(hash.hex)),
                    "next" -> Str(nextRole.shortName)
                  )
                }

            }

            val optionalsJson: Vector[(String, Num)] = {
              val fee = psbt.feeOpt.map(fee =>
                "fee" -> Num(fee.satoshis.toLong.toDouble))
              val vsize =
                psbt.estimateVSize.map(vsize =>
                  "estimated_vsize" -> Num(vsize.toDouble))
              val feeRate = psbt.estimateSatsPerVByte.map(feeRate =>
                "estimated_sats_vbyte" -> Num(feeRate.toLong.toDouble))

              Vector(fee, vsize, feeRate).flatten
            }

            val inputJson = Vector("inputs" -> Arr.from(inputs))
            val nextRoleJson: Vector[(String, Str)] =
              Vector("next" -> Str(psbt.nextRole.shortName))

            val jsonVec: Vector[(String, Value)] =
              inputJson ++ optionalsJson ++ nextRoleJson
            val jsonMap = mutable.LinkedHashMap(jsonVec: _*)
            val json = Obj(jsonMap)

            Server.httpSuccess(json)
          }
      }

    case ServerCommand("createmultisig", arr) =>
      withValidServerCommand(CreateMultisig.fromJsArr(arr)) {
        case CreateMultisig(requiredKeys, keys, addressType) =>
          complete {
            val sorted = keys.sortBy(_.hex)
            val spk = MultiSignatureScriptPubKey(requiredKeys, sorted)

            val address = addressType match {
              case AddressType.SegWit =>
                val p2wsh = P2WSHWitnessSPKV0(spk)
                Bech32Address(p2wsh, config.network)
              case AddressType.NestedSegWit =>
                val p2wsh = P2WSHWitnessSPKV0(spk)
                val p2sh = P2SHScriptPubKey(p2wsh)
                P2SHAddress(p2sh, config.network)
              case AddressType.Legacy =>
                val p2sh = P2SHScriptPubKey(spk)
                P2SHAddress(p2sh, config.network)
            }

            val json = Obj(
              "address" -> Str(address.toString),
              "redeemScript" -> Str(spk.hex)
            )
            Server.httpSuccess(json)
          }
      }
  }
}
