package org.bitcoins.dlc

import org.bitcoins.core.crypto.{ECPublicKey, ExtPublicKey, Sha256DigestBE}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.protocol.{Bech32Address, BlockStamp, NetworkElement}
import org.bitcoins.core.psbt.InputPSBTRecord
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.Factory
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import scodec.bits.ByteVector
import ujson._

import scala.collection.mutable

sealed trait DLCMessage

object DLCMessage {
  case class OracleInfo(pubKey: ECPublicKey, rValue: ECPublicKey)
      extends NetworkElement {

    override def bytes: ByteVector = pubKey.bytes ++ rValue.bytes
  }

  object OracleInfo extends Factory[OracleInfo] {

    override def fromBytes(bytes: ByteVector): OracleInfo = {
      require(bytes.size == 66, s"OracleInfo is only 66 bytes, got $bytes")

      val pubkey = ECPublicKey(bytes.take(33))
      val rValue = ECPublicKey(bytes.drop(33))

      // TODO: validate rValue once the libsecp functionality is added

      OracleInfo(pubkey, rValue)
    }
  }

  case class DLCOffer(
      contractInfo: Map[Sha256DigestBE, Satoshis],
      oracleInfo: OracleInfo,
      extPubKey: ExtPublicKey,
      totalCollateral: Satoshis,
      fundingInputs: Vector[(TransactionOutPoint, TransactionOutput)],
      changeAddress: Bech32Address,
      feeRate: SatoshisPerVirtualByte,
      timeouts: DLCTimeouts)
      extends DLCMessage {

    def toJson: Value = {
      val contractInfosJson =
        contractInfo
          .map(
            info =>
              mutable.LinkedHashMap("sha256" -> Str(info._1.hex),
                                    "sats" -> Num(info._2.toLong)))

      val fundingInputsJson =
        fundingInputs
          .map(
            input =>
              mutable.LinkedHashMap("outpoint" -> Str(input._1.hex),
                                    "output" -> Str(input._2.hex)))

      val timeoutsJson =
        mutable.LinkedHashMap(
          "penalty" -> Num(timeouts.penaltyTimeout),
          "contractMaturity" -> Num(timeouts.contractMaturity.toUInt32.toLong),
          "contractTimeout" -> Num(timeouts.contractTimeout.toUInt32.toLong)
        )

      Obj(
        mutable.LinkedHashMap[String, Value](
          "contractInfo" -> contractInfosJson,
          "oracleInfo" -> Str(oracleInfo.hex),
          "extPubKey" -> Str(extPubKey.hex),
          "totalCollateral" -> Num(totalCollateral.toLong),
          "fundingInputs" -> fundingInputsJson,
          "changeAddress" -> Str(changeAddress.value),
          "feeRate" -> Num(feeRate.toLong),
          "timeouts" -> timeoutsJson
        )
      )
    }

    def toJsonStr: String = toJson.toString()
  }

  object DLCOffer {

    def fromJson(js: Value): DLCOffer = {
      val iter = js.obj.iterator

      val emptyOffer =
        DLCOffer(Map.empty,
                 null,
                 null,
                 Satoshis.zero,
                 Vector.empty,
                 null,
                 SatoshisPerVirtualByte.one,
                 DLCTimeouts(0, BlockStamp(0), BlockStamp(0)))

      iter.foldLeft(emptyOffer) {
        case (offer, (key, value)) =>
          key match {
            case "contractInfo" =>
              if (value.arr.isEmpty) {
                offer
              } else {
                val contractInfo =
                  value.arr.map { subVal =>
                    val obj = subVal.obj
                    val sha256Index =
                      obj.keys.toList.indexOf("sha256")
                    val satsIndex = obj.keys.toList.indexOf("sats")

                    (Sha256DigestBE(obj.values(sha256Index).str),
                     Satoshis(obj.values(satsIndex).num.toLong))
                  }
                offer.copy(contractInfo = contractInfo.toMap)
              }
            case "oracleInfo" =>
              val oracleInfo = OracleInfo(value.str)
              offer.copy(oracleInfo = oracleInfo)
            case "extPubKey" =>
              val extPubKey = ExtPublicKey(value.str)
              offer.copy(extPubKey = extPubKey)
            case "totalCollateral" =>
              val totalCollateral = Satoshis(value.num.toLong)
              offer.copy(totalCollateral = totalCollateral)
            case "fundingInputs" =>
              if (value.arr.isEmpty) {
                offer
              } else {
                val fundingInputs = value.arr.map { subVal =>
                  val obj = subVal.obj
                  val outpointIndex =
                    obj.value.keys.toList.indexOf("outpoint")
                  val outputIndex = obj.value.keys.toList.indexOf("output")

                  (TransactionOutPoint(obj.values(outpointIndex).str),
                   TransactionOutput(obj.values(outputIndex).str))
                }
                offer.copy(fundingInputs = fundingInputs.toVector)
              }
            case "changeAddress" =>
              val changeAddress = Bech32Address.fromString(value.str).get
              offer.copy(changeAddress = changeAddress)
            case "feeRate" =>
              val feeRate = SatoshisPerVirtualByte(Satoshis(value.num.toLong))
              offer.copy(feeRate = feeRate)
            case "timeouts" =>
              val obj = value.obj
              val penaltyIndex =
                obj.keys.toList.indexOf("penalty")
              val contractMaturityIndex =
                obj.keys.toList.indexOf("contractMaturity")
              val contractTimeoutIndex =
                obj.keys.toList.indexOf("contractTimeout")

              val timeouts = DLCTimeouts(
                obj.values(penaltyIndex).num.toInt,
                BlockTime(UInt32(obj.values(contractMaturityIndex).num.toLong)),
                BlockTime(UInt32(obj.values(contractTimeoutIndex).num.toLong))
              )
              offer.copy(timeouts = timeouts)
            case other =>
              throw new RuntimeException(s"Received invalid key: $other")
          }
      }
    }
  }

  private def partialSigFromHex(hex: String): PartialSignature = {
    InputPSBTRecord(hex) match {
      case partialSignature: PartialSignature =>
        partialSignature
      case other =>
        throw new IllegalArgumentException(
          s"Invalid PartialSignature encoding, got: $other")
    }
  }

  case class DLCAccept(
      totalCollateral: Satoshis,
      extPubKey: ExtPublicKey,
      fundingInputs: Vector[(TransactionOutPoint, TransactionOutput)],
      changeAddress: Bech32Address,
      cetSigs: CETSignatures)
      extends DLCMessage {

    def toJson: Value = {
      val fundingInputsJson =
        fundingInputs.map(
          input =>
            mutable.LinkedHashMap("outpoint" -> Str(input._1.hex),
                                  "output" -> Str(input._2.hex)))

      val cetSigsJson =
        mutable.LinkedHashMap("winSig" -> Str(cetSigs.winSig.hex),
                              "loseSig" -> Str(cetSigs.loseSig.hex),
                              "refundSig" -> Str(cetSigs.refundSig.hex))

      Obj(
        mutable.LinkedHashMap[String, Value](
          "totalCollateral" -> Num(totalCollateral.toLong),
          "extPubKey" -> Str(extPubKey.hex),
          "fundingInputs" -> fundingInputsJson,
          "changeAddress" -> Str(changeAddress.value),
          "cetSigs" -> cetSigsJson
        )
      )
    }

    def toJsonStr: String = toJson.toString()
  }

  object DLCAccept {

    def fromJson(js: Value): DLCAccept = {
      val iter = js.obj.iterator

      val emptyAccept =
        DLCAccept(Satoshis.zero, null, Vector.empty, null, null)

      iter.foldLeft(emptyAccept) {
        case (accept, (key, value)) =>
          key match {
            case "totalCollateral" =>
              val totalCollateral = Satoshis(value.num.toLong)
              accept.copy(totalCollateral = totalCollateral)
            case "extPubKey" =>
              val extPubKey = ExtPublicKey(value.str)
              accept.copy(extPubKey = extPubKey)
            case "fundingInputs" =>
              if (value.arr.isEmpty) {
                accept
              } else {
                val fundingInputs = value.arr.map { subVal =>
                  val obj = subVal.obj
                  val outpointIndex =
                    obj.value.keys.toList.indexOf("outpoint")
                  val outputIndex = obj.value.keys.toList.indexOf("output")

                  (TransactionOutPoint(obj.values(outpointIndex).str),
                   TransactionOutput(obj.values(outputIndex).str))
                }
                accept.copy(fundingInputs = fundingInputs.toVector)
              }
            case "changeAddress" =>
              val changeAddress = Bech32Address.fromString(value.str).get
              accept.copy(changeAddress = changeAddress)
            case "cetSigs" =>
              val obj = value.obj
              val winIndex = obj.keys.toList.indexOf("winSig")
              val loseIndex = obj.keys.toList.indexOf("loseSig")
              val refundIndex = obj.keys.toList.indexOf("refundSig")

              val cetSigs = CETSignatures(
                partialSigFromHex(obj.values(winIndex).str),
                partialSigFromHex(obj.values(loseIndex).str),
                partialSigFromHex(obj.values(refundIndex).str)
              )
              accept.copy(cetSigs = cetSigs)
          }
      }
    }
  }

  case class DLCSign(cetSigs: CETSignatures, fundingSigs: FundingSignatures)
      extends DLCMessage {

    def toJson: Value = {
      val fundingSigsJson = fundingSigs.sigs.map(sig => Str(sig.hex))

      Obj(
        mutable.LinkedHashMap[String, Value](
          "winSig" -> Str(cetSigs.winSig.hex),
          "loseSig" -> Str(cetSigs.loseSig.hex),
          "refundSig" -> Str(cetSigs.refundSig.hex),
          "fundingSigs" -> fundingSigsJson
        )
      )
    }

    def toJsonStr: String = toJson.toString()
  }

  object DLCSign {

    def fromJson(js: Value): DLCSign = {
      val iter = js.obj.iterator

      val emptySign =
        DLCSign(CETSignatures(null, null, null),
                FundingSignatures(Vector.empty))

      iter.foldLeft(emptySign) {
        case (sign, (key, value)) =>
          key match {
            case "winSig" =>
              val winSig = partialSigFromHex(value.str)
              val cetSigs = sign.cetSigs.copy(winSig = winSig)
              sign.copy(cetSigs = cetSigs)
            case "loseSig" =>
              val loseSig = partialSigFromHex(value.str)
              val cetSigs = sign.cetSigs.copy(loseSig = loseSig)
              sign.copy(cetSigs = cetSigs)
            case "refundSig" =>
              val refundSig = partialSigFromHex(value.str)
              val cetSigs = sign.cetSigs.copy(refundSig = refundSig)
              sign.copy(cetSigs = cetSigs)
            case "fundingSigs" =>
              if (value.arr.isEmpty) {
                sign
              } else {
                val fundingSigs =
                  value.arr.map(subVal => partialSigFromHex(subVal.str))
                sign.copy(fundingSigs = FundingSignatures(fundingSigs.toVector))
              }
          }
      }
    }
  }
}
