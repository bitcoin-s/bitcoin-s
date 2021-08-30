package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import org.bitcoins.commons.jsonmodels.{SerializedPSBT, SerializedTransaction}
import org.bitcoins.core.hd.AddressType
import org.bitcoins.core.protocol.dlc.models.DLCMessage._
import org.bitcoins.core.protocol.script.{
  MultiSignatureScriptPubKey,
  P2SHScriptPubKey,
  P2WSHWitnessSPKV0
}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.{Bech32Address, P2SHAddress}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.server.BitcoinSAppConfig.toChainConf
import org.bitcoins.server.routes.{Server, ServerCommand, ServerRoute}
import ujson._

import java.util.Date
import scala.collection.mutable
import scala.concurrent.Future

case class CoreRoutes()(implicit system: ActorSystem, config: BitcoinSAppConfig)
    extends ServerRoute {
  import system.dispatcher

  def handleCommand: PartialFunction[ServerCommand, Route] = {
    case ServerCommand("finalizepsbt", arr) =>
      withValidServerCommand(FinalizePSBT.fromJsArr(arr)) {
        case FinalizePSBT(psbt) =>
          complete {
            psbt.finalizePSBT
              .map(finalized => Server.httpSuccess(finalized.base64))
          }
      }

    case ServerCommand("extractfrompsbt", arr) =>
      withValidServerCommand(ExtractFromPSBT.fromJsArr(arr)) {
        case ExtractFromPSBT(psbt) =>
          complete {
            psbt.extractTransactionAndValidate
              .map(tx => Server.httpSuccess(tx.hex))
          }
      }

    case ServerCommand("converttopsbt", arr) =>
      withValidServerCommand(ConvertToPSBT.fromJsArr(arr)) {
        case ConvertToPSBT(tx) =>
          complete {
            val psbt = PSBT.fromUnsignedTx(tx)

            Server.httpSuccess(psbt.base64)
          }
      }

    case ServerCommand("combinepsbts", arr) =>
      withValidServerCommand(CombinePSBTs.fromJsArr(arr)) {
        case CombinePSBTs(psbts) =>
          complete {
            combinePSBTs(psbts.toVector)
              .map(psbt => Server.httpSuccess(psbt.base64))
          }
      }

    case ServerCommand("joinpsbts", arr) =>
      withValidServerCommand(JoinPSBTs.fromJsArr(arr)) {
        case JoinPSBTs(psbts) =>
          complete {
            combinePSBTs(psbts.toVector)
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

    case ServerCommand("decodeoffer", arr) =>
      withValidServerCommand(DecodeOffer.fromJsArr(arr)) {
        case DecodeOffer(offerTLV) =>
          complete {
            val offer = DLCOffer.fromTLV(offerTLV)

            val fundingInputJson = offer.fundingInputs.map { input =>
              Obj(
                "inputSerialId" -> Str(input.inputSerialId.hex),
                "prevTx" -> Str(input.prevTx.hex),
                "prevTxId" -> Str(input.prevTx.txIdBE.hex),
                "vout" -> Num(input.prevTxVout.toLong.toDouble),
                "sequence" -> Num(input.sequence.toLong.toDouble)
              )
            }

            val json = Obj(
              "contractInfo" -> Str(offer.contractInfo.hex),
              "fundingKey" -> Str(offer.pubKeys.fundingKey.hex),
              "payoutAddress" -> Str(offer.pubKeys.payoutAddress.toString),
              "changeAddress" -> Str(offer.changeAddress.toString),
              "totalCollateral" -> Num(offer.totalCollateral.toLong.toDouble),
              "fundingInputs" -> fundingInputJson,
              "payoutSerialId" -> Str(offer.payoutSerialId.hex),
              "changeSerialId" -> Str(offer.changeSerialId.hex),
              "fundOutputSerialId" -> Str(offer.fundOutputSerialId.hex),
              "feeRate" -> Num(offer.feeRate.toLong.toDouble),
              "contractMaturity" -> Num(
                offer.timeouts.contractMaturity.toUInt32.toLong.toDouble),
              "contractTimeout" -> Num(
                offer.timeouts.contractTimeout.toUInt32.toLong.toDouble)
            )

            Server.httpSuccess(json)
          }
      }

    case ServerCommand("decodeannouncement", arr) =>
      withValidServerCommand(DecodeAnnouncement.fromJsArr(arr)) {
        case DecodeAnnouncement(announcement) =>
          complete {
            val noncesJson = announcement.eventTLV.nonces.map { nonce =>
              Str(nonce.hex)
            }

            val descriptorJson = announcement.eventTLV.eventDescriptor match {
              case EnumEventDescriptorV0TLV(outcomes) =>
                Obj(
                  "outcomes" -> outcomes.map(Str(_))
                )
              case numeric: NumericEventDescriptorTLV =>
                Obj(
                  "base" -> Num(numeric.base.toLong.toDouble),
                  "isSigned" -> Bool(numeric.isSigned),
                  "unit" -> Str(numeric.unit),
                  "precision" -> Num(numeric.precision.toLong.toDouble)
                )
            }

            val eventJson = Obj(
              "nonces" -> noncesJson,
              "maturity" -> Str(
                TimeUtil.iso8601ToString(
                  Date.from(announcement.eventTLV.maturation))),
              "descriptor" -> descriptorJson,
              "eventId" -> Str(announcement.eventTLV.eventId)
            )

            val json = Obj(
              "announcementSignature" -> Str(
                announcement.announcementSignature.hex),
              "publicKey" -> Str(announcement.publicKey.hex),
              "event" -> eventJson
            )

            Server.httpSuccess(json)
          }
      }

    case ServerCommand("decodeattestments", arr) =>
      withValidServerCommand(DecodeAttestations.fromJsArr(arr)) {
        case DecodeAttestations(attestments) =>
          complete {
            val sigsJson = attestments.sigs.map { sig =>
              Str(sig.hex)
            }

            val valuesJson = attestments.outcomes.map { value =>
              Str(value)
            }

            val json = Obj(
              "eventId" -> Str(attestments.eventId),
              "signatures" -> sigsJson,
              "values" -> valuesJson
            )

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

    case ServerCommand("zipdatadir", arr) =>
      withValidServerCommand(ZipDataDir.fromJsArr(arr)) {
        case ZipDataDir(path) =>
          complete {
            config.zipDatadir(path)
            Server.httpSuccess(ujson.Null)
          }
      }
  }

  def combinePSBTs(psbts: Vector[PSBT]): Future[PSBT] = {
    if (psbts.isEmpty) {
      Future.failed(new IllegalArgumentException("No PSBTs given"))
    } else {
      try {
        val empty = PSBT.fromUnsignedTx(psbts.head.transaction)
        val combined =
          psbts.foldLeft(empty)((accum, psbt) => accum.combinePSBT(psbt))

        Future.successful(combined)
      } catch {
        case err: IllegalArgumentException =>
          Future.failed(err)
      }
    }
  }
}
