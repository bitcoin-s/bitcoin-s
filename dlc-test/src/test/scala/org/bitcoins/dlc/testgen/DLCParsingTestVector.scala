package org.bitcoins.dlc.testgen

import org.bitcoins.core.number.{UInt16, UInt64}
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto.{CryptoUtil, NetworkElement}
import org.bitcoins.dlc.testgen.ByteVectorWrapper._
import play.api.libs.json._
import scodec.bits.ByteVector

sealed trait DLCParsingTestVector extends TestVector {
  def input: NetworkElement
  def tpeName: String
  def fields: Vector[(String, ByteVectorWrapper)]

  override def toJson: JsValue = {
    val jsonFields = fields.map { case (name, byteWrapper) =>
      name -> byteWrapper.toJson
    }

    JsObject(
      Map(
        "tpeName" -> JsString(tpeName),
        "input" -> JsString(input.hex),
        "fields" -> JsObject(jsonFields)
      ))
  }
}

sealed trait ByteVectorWrapper {
  def toJson: JsValue
}

object ByteVectorWrapper {

  implicit class Element(val bytes: ByteVector) extends ByteVectorWrapper {

    override def equals(obj: Any): Boolean = {
      obj match {
        case elem: Element => elem.bytes == bytes
        case _             => false
      }
    }

    override def toString: String = {
      s"Element(${bytes.length} bytes, 0x${bytes.toHex})"
    }

    override def toJson: JsValue = {
      JsString(bytes.toHex)
    }
  }

  object Element {

    def apply(element: NetworkElement): Element = {
      element.bytes
    }
  }

  case class MultiElement(elements: Vector[ByteVectorWrapper])
      extends ByteVectorWrapper {

    override def toString: String = {
      s"MultiElement(${elements.mkString(",")})"
    }

    override def toJson: JsValue = {
      JsArray(elements.map(_.toJson))
    }
  }

  object MultiElement {

    def apply(elements: ByteVectorWrapper*): MultiElement = {
      new MultiElement(elements.toVector)
    }
  }

  case class NamedMultiElement(elements: Vector[(String, ByteVectorWrapper)])
      extends ByteVectorWrapper {

    override def toString: String = {
      s"NamedMultiElement(${elements
        .map { case (name, bytes) => s"$name -> $bytes" }
        .mkString(",")})"
    }

    override def toJson: JsValue = {
      JsObject(elements.map { case (name, element) => name -> element.toJson })
    }
  }

  object NamedMultiElement {

    def apply(elements: (String, ByteVectorWrapper)*): NamedMultiElement = {
      new NamedMultiElement(elements.toVector)
    }
  }

  def fromJson(json: JsValue): JsResult[ByteVectorWrapper] = {
    json.validate[String] match {
      case JsSuccess(hex, _) => JsSuccess(ByteVector.fromValidHex(hex))
      case JsError(_) =>
        json.validate[Vector[JsValue]] match {
          case JsSuccess(vec, _) =>
            val nestedResult =
              DLCParsingTestVector.flattenJsResult(vec.map(fromJson))
            nestedResult.map(MultiElement(_))
          case JsError(_) =>
            json.validate[Map[String, JsValue]] match {
              case JsSuccess(obj, _) =>
                val jsResults = obj.map { case (name, nestedJson) =>
                  fromJson(nestedJson).map(name -> _)
                }.toVector
                val nestedResult =
                  DLCParsingTestVector.flattenJsResult(jsResults)
                nestedResult.map(NamedMultiElement(_))
              case JsError(_) => JsError("Couldn't parse field bytes")
            }
        }
    }
  }
}

case class DLCTLVTestVector(
    input: TLV,
    tpeName: String,
    fields: Vector[(String, ByteVectorWrapper)])
    extends DLCParsingTestVector

case class DLCMessageTestVector(
    input: LnMessage[TLV],
    tpeName: String,
    fields: Vector[(String, ByteVectorWrapper)])
    extends DLCParsingTestVector

object DLCParsingTestVector extends TestVectorParser[DLCParsingTestVector] {

  def apply(tlv: TLV): DLCParsingTestVector = {
    tlv match {
      case PayoutFunctionV0TLV(points) =>
        val fields = Vector(
          "tpe" -> Element(PayoutFunctionV0TLV.tpe),
          "length" -> Element(tlv.length),
          "numPoints" -> Element(UInt16(points.length)),
          "points" -> MultiElement(points.map { point =>
            NamedMultiElement(
              "isEndpoint" -> Element(ByteVector(point.leadingByte)),
              "outcome" -> Element(BigSizeUInt(point.outcome)),
              "value" -> Element(UInt64(point.value.toLong))
            )
          })
        )
        DLCTLVTestVector(tlv, "payout_function_v0", fields)
      case RoundingIntervalsV0TLV(intervalStarts) =>
        val fields = Vector(
          "tpe" -> Element(RoundingIntervalsV0TLV.tpe),
          "length" -> Element(tlv.length),
          "numIntervals" -> Element(UInt16(intervalStarts.length)),
          "intervals" -> MultiElement(intervalStarts.map {
            case (intervalStart, roundingMod) =>
              NamedMultiElement(
                "beginInterval" -> Element(BigSizeUInt(intervalStart)),
                "roundingMod" -> Element(BigSizeUInt(roundingMod.toLong))
              )
          })
        )
        DLCTLVTestVector(tlv, "rounding_intervals_v0", fields)
      case ContractDescriptorV0TLV(outcomes) =>
        val fields = Vector(
          "tpe" -> Element(ContractDescriptorV0TLV.tpe),
          "length" -> Element(tlv.length),
          "outcomes" -> MultiElement(outcomes.map { case (outcome, amt) =>
            NamedMultiElement("outcome" -> CryptoUtil.sha256(outcome).bytes,
                              "localPayout" -> amt.toUInt64.bytes)
          })
        )
        DLCTLVTestVector(tlv, "contract_descriptor_v0", fields)
      case ContractDescriptorV1TLV(numDigits,
                                   payoutFunction,
                                   roundingIntervals) =>
        val fields = Vector(
          "tpe" -> Element(ContractDescriptorV1TLV.tpe),
          "length" -> Element(tlv.length),
          "numDigits" -> Element(UInt16(numDigits)),
          "payoutFunction" -> Element(payoutFunction),
          "roundingIntervals" -> Element(roundingIntervals)
        )
        DLCTLVTestVector(tlv, "contract_descriptor_v1", fields)
      case OracleInfoV0TLV(announcement) =>
        val fields = Vector(
          "tpe" -> Element(OracleInfoV0TLV.tpe),
          "length" -> Element(tlv.length),
          "announcement" -> Element(announcement)
        )
        DLCTLVTestVector(tlv, "oracle_info_v0", fields)
      case OracleParamsV0TLV(maxErrorExp, minFailExp, maximizeCoverage) =>
        val maximizeCoverageBytes = ByteVector(
          if (maximizeCoverage) TLV.TRUE_BYTE else TLV.FALSE_BYTE)

        val fields = Vector(
          "tpe" -> Element(OracleParamsV0TLV.tpe),
          "length" -> Element(tlv.length),
          "maxErrorExp" -> Element(UInt16(maxErrorExp)),
          "minFailExp" -> Element(UInt16(minFailExp)),
          "maximizeCoverage" -> Element(maximizeCoverageBytes)
        )
        DLCTLVTestVector(tlv, "oracle_params_v0", fields)
      case OracleInfoV1TLV(threshold, announcements) =>
        val fields = Vector(
          "tpe" -> Element(OracleInfoV1TLV.tpe),
          "length" -> Element(tlv.length),
          "threshold" -> Element(UInt16(threshold)),
          "announcements" -> MultiElement(
            announcements.toVector.map(Element(_)))
        )
        DLCTLVTestVector(tlv, "oracle_info_v1", fields)
      case OracleInfoV2TLV(threshold, oracles, params) =>
        val fields = Vector(
          "tpe" -> Element(OracleInfoV2TLV.tpe),
          "length" -> Element(tlv.length),
          "threshold" -> Element(UInt16(threshold)),
          "announcements" -> MultiElement(oracles.toVector.map(Element(_))),
          "params" -> Element(params)
        )
        DLCTLVTestVector(tlv, "oracle_info_v2", fields)
      case ContractInfoV0TLV(totalCollateral, contractDescriptor, oracleInfo) =>
        val fields = Vector(
          "tpe" -> Element(ContractInfoV0TLV.tpe),
          "length" -> Element(tlv.length),
          "totalCollateral" -> Element(totalCollateral.toUInt64),
          "contractDescriptor" -> Element(contractDescriptor),
          "oracleInfo" -> Element(oracleInfo)
        )
        DLCTLVTestVector(tlv, "contract_info_v0", fields)
      case FundingInputV0TLV(inputSerialId,
                             prevTx,
                             prevTxVout,
                             sequence,
                             maxWitnessLen,
                             redeemScriptOpt) =>
        val redeemScript =
          redeemScriptOpt.getOrElse(EmptyScriptPubKey)

        val fields = Vector(
          "tpe" -> Element(FundingInputV0TLV.tpe),
          "length" -> Element(tlv.length),
          "inputSerialId" -> Element(inputSerialId),
          "prevTxLen" -> Element(UInt16(prevTx.byteSize)),
          "prevTx" -> Element(prevTx),
          "prevTxVout" -> Element(prevTxVout),
          "sequence" -> Element(sequence),
          "maxWitnessLen" -> Element(maxWitnessLen),
          "redeemScriptLen" -> Element(UInt16(redeemScript.asmBytes.length)),
          "redeemScript" -> Element(redeemScript.asmBytes)
        )
        DLCTLVTestVector(tlv, "funding_input_v0", fields)
      case CETSignaturesV0TLV(sigs) =>
        val fields = Vector(
          "tpe" -> Element(CETSignaturesV0TLV.tpe),
          "length" -> Element(tlv.length),
          "sigs" -> MultiElement(
            sigs.map(sig =>
              NamedMultiElement("encryptedSig" -> sig.adaptedSig,
                                "dleqProof" -> sig.dleqProof)))
        )
        DLCTLVTestVector(tlv, "cet_adaptor_signatures_v0", fields)
      case FundingSignaturesV0TLV(witnesses) =>
        val fields = Vector(
          "tpe" -> Element(FundingSignaturesV0TLV.tpe),
          "length" -> Element(tlv.length),
          "numWitnesses" -> Element(UInt16(witnesses.length)),
          "witnesses" -> MultiElement(witnesses.map { witness =>
            NamedMultiElement(
              "stackLen" -> Element(UInt16(witness.stack.length)),
              "stack" -> MultiElement(witness.stack.toVector.reverse.map {
                stackElem =>
                  NamedMultiElement(
                    "stackElementLen" -> Element(UInt16(stackElem.length)),
                    "stackElement" -> stackElem)
              })
            )
          })
        )
        DLCTLVTestVector(tlv, "funding_signatures_v0", fields)
      case DLCOfferTLV(contractFlags,
                       chainHash,
                       contractInfo,
                       fundingPubKey,
                       payoutSPK,
                       payoutSerialId,
                       totalCollateralSatoshis,
                       fundingInputs,
                       changeSPK,
                       changeSerialId,
                       fundOutputSerialId,
                       feeRate,
                       contractMaturityBound,
                       contractTimeout) =>
        val fields = Vector(
          "tpe" -> Element(UInt16(DLCOfferTLV.tpe.toInt)),
          "contractFlags" -> Element(ByteVector(contractFlags)),
          "chainHash" -> Element(chainHash),
          "contractInfo" -> Element(contractInfo),
          "fundingPubKey" -> Element(fundingPubKey),
          "payoutSPKLen" -> Element(UInt16(payoutSPK.asmBytes.length)),
          "payoutSPK" -> Element(payoutSPK.asmBytes),
          "payoutSerialId" -> Element(payoutSerialId),
          "totalCollateralSatoshis" -> Element(
            totalCollateralSatoshis.toUInt64),
          "fundingInputsLen" -> Element(UInt16(fundingInputs.length)),
          "fundingInputs" -> new MultiElement(
            fundingInputs.map(input => Element(input.bytes))),
          "changeSPKLen" -> Element(UInt16(changeSPK.asmBytes.length)),
          "changeSPK" -> Element(changeSPK.asmBytes),
          "changeSerialId" -> Element(changeSerialId),
          "fundOutputSerialId" -> Element(fundOutputSerialId),
          "feeRate" -> Element(feeRate.currencyUnit.satoshis.toUInt64),
          "contractMaturityBound" -> Element(contractMaturityBound.toUInt32),
          "contractTimeout" -> Element(contractTimeout.toUInt32)
        )
        DLCMessageTestVector(LnMessage(tlv), "offer_dlc_v0", fields)
      case NoNegotiationFieldsTLV =>
        val fields = Vector(
          "tpe" -> Element(NoNegotiationFieldsTLV.tpe),
          "length" -> Element(tlv.length)
        )
        DLCTLVTestVector(tlv, "no_negotiation_fields", fields)
      case NegotiationFieldsV1TLV(roundingIntervalsV0TLV) =>
        val fields = Vector(
          "tpe" -> Element(NegotiationFieldsV1TLV.tpe),
          "length" -> Element(tlv.length),
          "rounding_intervals_v0" -> Element(roundingIntervalsV0TLV)
        )
        DLCTLVTestVector(tlv, "negotiation_fields_v1", fields)
      case DLCAcceptTLV(tempContractId,
                        totalCollateralSatoshis,
                        fundingPubKey,
                        payoutSPK,
                        payoutSerialId,
                        fundingInputs,
                        changeSPK,
                        changeSerialId,
                        cetSignatures,
                        refundSignature,
                        negotiationFields) =>
        val fields = Vector(
          "tpe" -> Element(UInt16(DLCAcceptTLV.tpe.toInt)),
          "tempContractId" -> Element(tempContractId),
          "totalCollateralSatoshis" -> Element(
            totalCollateralSatoshis.toUInt64),
          "fundingPubKey" -> Element(fundingPubKey),
          "payoutSPKLen" -> Element(UInt16(payoutSPK.asmBytes.length)),
          "payoutSPK" -> Element(payoutSPK.asmBytes),
          "payoutSerialId" -> Element(payoutSerialId),
          "fundingInputsLen" -> Element(UInt16(fundingInputs.length)),
          "fundingInputs" -> new MultiElement(
            fundingInputs.map(input => Element(input.bytes))),
          "changeSPKLen" -> Element(UInt16(changeSPK.asmBytes.length)),
          "changeSPK" -> Element(changeSPK.asmBytes),
          "changeSerialId" -> Element(changeSerialId),
          "cetSignatures" -> Element(cetSignatures),
          "refundSignature" -> Element(refundSignature.toRawRS),
          "negotiationFields" -> Element(negotiationFields)
        )
        DLCMessageTestVector(LnMessage(tlv), "accept_dlc_v0", fields)
      case DLCSignTLV(contractId,
                      cetSignatures,
                      refundSignature,
                      fundingSignatures) =>
        val fields = Vector(
          "tpe" -> Element(UInt16(DLCSignTLV.tpe.toInt)),
          "contractId" -> Element(contractId),
          "cetSignatures" -> Element(cetSignatures),
          "refundSignature" -> Element(refundSignature.toRawRS),
          "fundingSignatures" -> Element(fundingSignatures)
        )
        DLCMessageTestVector(LnMessage(tlv), "sign_dlc_v0", fields)
      case EnumEventDescriptorV0TLV(outcomes) =>
        val fields = Vector(
          "tpe" -> Element(EnumEventDescriptorV0TLV.tpe),
          "length" -> Element(tlv.length),
          "numOutcomes" -> Element(UInt16(outcomes.size)),
          "outcomes" -> MultiElement(outcomes.map { outcome =>
            val outcomeBytes = CryptoUtil.serializeForHash(outcome)
            NamedMultiElement(
              "outcomeLen" -> Element(UInt16(outcomeBytes.length)),
              "outcome" -> Element(outcomeBytes))
          })
        )

        DLCTLVTestVector(tlv, "enum_event_descriptor_v0", fields)
      case SignedDigitDecompositionEventDescriptor(base,
                                                   numDigits,
                                                   units,
                                                   precision) =>
        val fields = Vector(
          "tpe" -> Element(DigitDecompositionEventDescriptorV0TLV.tpe),
          "length" -> Element(tlv.length),
          "base" -> Element(base),
          "numDigits" -> Element(numDigits),
          "isSigned" -> Element(ByteVector.fromByte(0x01)),
          "units" -> Element(CryptoUtil.serializeForHash(units)),
          "precision" -> Element(precision)
        )

        DLCTLVTestVector(tlv, "digit_decomp_event_descriptor_v0", fields)
      case UnsignedDigitDecompositionEventDescriptor(base,
                                                     numDigits,
                                                     units,
                                                     precision) =>
        val fields = Vector(
          "tpe" -> Element(DigitDecompositionEventDescriptorV0TLV.tpe),
          "length" -> Element(tlv.length),
          "base" -> Element(base),
          "numDigits" -> Element(numDigits),
          "isSigned" -> Element(ByteVector.fromByte(0x00)),
          "units" -> Element(CryptoUtil.serializeForHash(units)),
          "precision" -> Element(precision)
        )

        DLCTLVTestVector(tlv, "digit_decomp_event_descriptor_v0", fields)
      case OracleEventV0TLV(nonces, eventMaturity, descriptor, uri) =>
        val fields = Vector(
          "tpe" -> Element(OracleEventV0TLV.tpe),
          "length" -> Element(tlv.length),
          "oracleNonces" -> MultiElement(nonces.vec.map(Element(_))),
          "eventMaturityEpoch" -> Element(eventMaturity),
          "eventDescriptor" -> Element(descriptor),
          "event_uri" -> Element(CryptoUtil.serializeForHash(uri))
        )

        DLCTLVTestVector(tlv, "oracle_event_v0", fields)
      case OracleAnnouncementV0TLV(sig, pubkey, event) =>
        val fields = Vector(
          "tpe" -> Element(UInt16(OracleAnnouncementV0TLV.tpe.toInt)),
          "length" -> Element(tlv.length),
          "signature" -> Element(sig),
          "oraclePubKey" -> Element(pubkey),
          "oracleEvent" -> Element(event)
        )

        DLCMessageTestVector(LnMessage(tlv), "oracle_announcement_v0", fields)
      case OracleAttestmentV0TLV(eventId, pubkey, sigs, outcomes) =>
        val fields = Vector(
          "tpe" -> Element(UInt16(OracleAttestmentV0TLV.tpe.toInt)),
          "length" -> Element(tlv.length),
          "eventId" -> Element(eventId),
          "oraclePubKey" -> Element(pubkey),
          "signatures" -> MultiElement(sigs.map(Element(_))),
          "outcomes" -> MultiElement(outcomes.map(Element(_)))
        )

        DLCMessageTestVector(LnMessage(tlv), "oracle_attestment_v0", fields)
      case _: UnknownTLV | _: ErrorTLV | _: PingTLV | _: PongTLV | _: InitTLV =>
        throw new IllegalArgumentException(
          s"DLCParsingTestVector is only defined for DLC messages and TLVs, got $tlv")
    }
  }

  def flattenJsResult[T](vec: Vector[JsResult[T]]): JsResult[Vector[T]] = {
    vec.foldLeft[JsResult[Vector[T]]](JsSuccess(Vector.empty)) {
      case (vecResult, elemResult) =>
        vecResult.flatMap { vec =>
          elemResult.map { elem =>
            vec :+ elem
          }
        }
    }
  }

  override def fromJson(json: JsValue): JsResult[DLCParsingTestVector] = {
    for {
      outer <- json.validate[Map[String, JsValue]]
      inputBytes <-
        outer("input").validate[String].map(ByteVector.fromValidHex(_))
      tpeName <- outer("tpeName").validate[String]
      jsFields <- outer("fields").validate[JsObject].map(_.fields.toVector)
      fields <- flattenJsResult {
        jsFields.map { case (name, field) =>
          ByteVectorWrapper.fromJson(field).map(name -> _)
        }
      }
    } yield {
      val msgTpe = UInt16(inputBytes.take(2)).toInt
      if (TLV.knownTypes.contains(BigSizeUInt(msgTpe))) {
        DLCMessageTestVector(LnMessage(inputBytes), tpeName, fields)
      } else {
        DLCTLVTestVector(TLV(inputBytes), tpeName, fields)
      }
    }
  }

  def tlvFromJson(input: JsValue): JsResult[TLV] = {
    val inputBytesResult =
      input.validate[String].map(ByteVector.fromValidHex(_))
    inputBytesResult.map { inputBytes =>
      val msgTpe = UInt16(inputBytes.take(2)).toInt
      if (TLV.knownTypes.contains(BigSizeUInt(msgTpe))) {
        LnMessage(inputBytes).tlv
      } else {
        TLV(inputBytes)
      }
    }
  }
}
