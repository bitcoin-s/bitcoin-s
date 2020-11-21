package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import org.bitcoins.commons.jsonmodels.{SerializedPSBT, SerializedTransaction}
import org.bitcoins.core.api.core.CoreApi
import ujson._

import scala.collection.mutable
import scala.util.{Failure, Success}

case class CoreRoutes(core: CoreApi)(implicit system: ActorSystem)
    extends ServerRoute {
  import system.dispatcher

  def handleCommand: PartialFunction[ServerCommand, StandardRoute] = {
    case ServerCommand("finalizepsbt", arr) =>
      FinalizePSBT.fromJsArr(arr) match {
        case Success(FinalizePSBT(psbt)) =>
          complete {
            core
              .finalizePSBT(psbt)
              .map(finalized => Server.httpSuccess(finalized.base64))
          }
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
      }

    case ServerCommand("extractfrompsbt", arr) =>
      ExtractFromPSBT.fromJsArr(arr) match {
        case Success(ExtractFromPSBT(psbt)) =>
          complete {
            core
              .extractFromPSBT(psbt)
              .map(tx => Server.httpSuccess(tx.hex))
          }
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
      }

    case ServerCommand("converttopsbt", arr) =>
      ConvertToPSBT.fromJsArr(arr) match {
        case Success(ConvertToPSBT(tx)) =>
          complete {
            core
              .convertToPSBT(tx)
              .map(psbt => Server.httpSuccess(psbt.base64))
          }
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
      }

    case ServerCommand("combinepsbts", arr) =>
      CombinePSBTs.fromJsArr(arr) match {
        case Success(CombinePSBTs(psbts)) =>
          complete {
            core
              .combinePSBTs(psbts)
              .map(psbt => Server.httpSuccess(psbt.base64))
          }
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
      }

    case ServerCommand("joinpsbts", arr) =>
      JoinPSBTs.fromJsArr(arr) match {
        case Success(JoinPSBTs(psbts)) =>
          complete {
            core
              .joinPSBTs(psbts)
              .map(psbt => Server.httpSuccess(psbt.base64))
          }
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
      }

    case ServerCommand("decoderawtransaction", arr) =>
      DecodeRawTransaction.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(DecodeRawTransaction(tx)) =>
          complete {
            val decoded = SerializedTransaction.decodeRawTransaction(tx)
            val uJson = ujson.read(decoded.toJson.toString())
            Server.httpSuccess(uJson)
          }
      }

    case ServerCommand("decodepsbt", arr) =>
      DecodePSBT.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(DecodePSBT(psbt)) =>
          complete {
            val decoded = SerializedPSBT.decodePSBT(psbt)
            val uJson = ujson.read(decoded.toJson.toString())
            Server.httpSuccess(uJson)
          }
      }

    case ServerCommand("analyzepsbt", arr) =>
      AnalyzePSBT.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(AnalyzePSBT(psbt)) =>
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
  }
}
