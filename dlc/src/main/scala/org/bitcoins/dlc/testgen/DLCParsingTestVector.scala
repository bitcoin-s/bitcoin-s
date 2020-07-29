package org.bitcoins.dlc.testgen

import org.bitcoins.core.number.UInt16
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto.NetworkElement
import org.bitcoins.dlc.testgen.ByteVectorWrapper._
import play.api.libs.json.{
  JsArray,
  JsError,
  JsObject,
  JsResult,
  JsString,
  JsSuccess,
  JsValue
}
import scodec.bits.ByteVector

sealed trait DLCParsingTestVector extends TestVector {
  def input: NetworkElement
  def tpeName: String
  def fields: Vector[(String, ByteVectorWrapper)]

  override def toJson: JsValue = {
    val jsonFields = fields.map {
      case (name, byteWrapper) =>
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
                val jsResults = obj.map {
                  case (name, nestedJson) => fromJson(nestedJson).map(name -> _)
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
      case ContractInfoV0TLV(outcomes) =>
        val fields = Vector(
          "tpe" -> Element(ContractInfoV0TLV.tpe),
          "length" -> Element(tlv.length),
          "outcomes" -> MultiElement(outcomes.toVector.map {
            case (outcome, amt) =>
              NamedMultiElement("outcome" -> outcome.bytes,
                                "localPayout" -> amt.toUInt64.bytes)
          })
        )
        DLCTLVTestVector(tlv, "contract_info_v0", fields)
      case OracleInfoV0TLV(pubKey, rValue) =>
        val fields = Vector(
          "tpe" -> Element(OracleInfoV0TLV.tpe),
          "length" -> Element(tlv.length),
          "pubKey" -> Element(pubKey),
          "rValue" -> Element(rValue)
        )
        DLCTLVTestVector(tlv, "oracle_info_v0", fields)
      case FundingInputV0TLV(prevTx,
                             prevTxVout,
                             sequence,
                             maxWitnessLen,
                             redeemScriptOpt) =>
        val redeemScript =
          redeemScriptOpt.getOrElse(EmptyScriptPubKey)

        val fields = Vector(
          "tpe" -> Element(FundingInputV0TLV.tpe),
          "length" -> Element(tlv.length),
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
              "stack" -> MultiElement(witness.stack.toVector.map { stackElem =>
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
                       oracleInfo,
                       fundingPubKey,
                       payoutSPK,
                       totalCollateralSatoshis,
                       fundingInputs,
                       changeSPK,
                       feeRate,
                       contractMaturityBound,
                       contractTimeout) =>
        val fields = Vector(
          "tpe" -> Element(UInt16(DLCOfferTLV.tpe.toInt)),
          "contractFlags" -> Element(ByteVector(contractFlags)),
          "chainHash" -> Element(chainHash),
          "contractInfo" -> Element(contractInfo),
          "oracleInfo" -> Element(oracleInfo),
          "fundingPubKey" -> Element(fundingPubKey),
          "payoutSPKLen" -> Element(UInt16(payoutSPK.asmBytes.length)),
          "payoutSPK" -> Element(payoutSPK.asmBytes),
          "totalCollateralSatoshis" -> Element(
            totalCollateralSatoshis.toUInt64),
          "fundingInputsLen" -> Element(UInt16(fundingInputs.length)),
          "fundingInputs" -> new MultiElement(
            fundingInputs.map(input => Element(input.bytes))),
          "changeSPKLen" -> Element(UInt16(changeSPK.asmBytes.length)),
          "changeSPK" -> Element(changeSPK.asmBytes),
          "feeRate" -> Element(feeRate.currencyUnit.satoshis.toUInt64),
          "contractMaturityBound" -> Element(contractMaturityBound.toUInt32),
          "contractTimeout" -> Element(contractTimeout.toUInt32)
        )
        DLCMessageTestVector(LnMessage(tlv), "offer_dlc_v0", fields)
      case DLCAcceptTLV(tempContractId,
                        totalCollateralSatoshis,
                        fundingPubKey,
                        payoutSPK,
                        fundingInputs,
                        changeSPK,
                        cetSignatures,
                        refundSignature) =>
        val fields = Vector(
          "tpe" -> Element(UInt16(DLCAcceptTLV.tpe.toInt)),
          "tempContractId" -> Element(tempContractId),
          "totalCollateralSatoshis" -> Element(
            totalCollateralSatoshis.toUInt64),
          "fundingPubKey" -> Element(fundingPubKey),
          "payoutSPKLen" -> Element(UInt16(payoutSPK.asmBytes.length)),
          "payoutSPK" -> Element(payoutSPK.asmBytes),
          "fundingInputsLen" -> Element(UInt16(fundingInputs.length)),
          "fundingInputs" -> new MultiElement(
            fundingInputs.map(input => Element(input.bytes))),
          "changeSPKLen" -> Element(UInt16(changeSPK.asmBytes.length)),
          "changeSPK" -> Element(changeSPK.asmBytes),
          "cetSignatures" -> Element(cetSignatures),
          "refundSignature" -> Element(refundSignature)
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
          "refundSignature" -> Element(refundSignature),
          "fundingSignatures" -> Element(fundingSignatures)
        )
        DLCMessageTestVector(LnMessage(tlv), "sign_dlc_v0", fields)
      case _: UnknownTLV | _: ErrorTLV | _: PingTLV | _: PongTLV =>
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
        jsFields.map {
          case (name, field) =>
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
