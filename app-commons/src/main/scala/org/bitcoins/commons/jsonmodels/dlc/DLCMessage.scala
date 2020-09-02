package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.core.config.{NetworkParameters, Networks}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.tlv.{
  CETSignaturesV0TLV,
  ContractInfoV0TLV,
  DLCAcceptTLV,
  DLCOfferTLV,
  DLCSignTLV,
  FundingInputTempTLV,
  FundingSignaturesV0TLV,
  OracleInfoV0TLV
}
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.MapWrapper
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import scodec.bits.ByteVector
import ujson._

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait DLCMessage {
  def toJson: Value
  def toJsonStr: String = toJson.toString()
}

object DLCMessage {

  private def getValue(key: String)(implicit
      obj: mutable.LinkedHashMap[String, Value]): Value = {
    val index = obj.keys.toList.indexOf(key)
    obj.values(index)
  }

  case class OracleInfo(pubKey: SchnorrPublicKey, rValue: SchnorrNonce)
      extends NetworkElement {

    override def bytes: ByteVector = pubKey.bytes ++ rValue.bytes

    def toTLV: OracleInfoV0TLV = OracleInfoV0TLV(pubKey, rValue)
  }

  object OracleInfo extends Factory[OracleInfo] {

    val dummy: OracleInfo = OracleInfo(ByteVector.fill(64)(1))

    override def fromBytes(bytes: ByteVector): OracleInfo = {
      require(bytes.size == 64, s"OracleInfo is only 64 bytes, got $bytes")

      val pubkey = SchnorrPublicKey(bytes.take(32))
      val rValue = SchnorrNonce(bytes.drop(32))

      OracleInfo(pubkey, rValue)
    }
  }

  case class ContractInfo(outcomeValueMap: Map[Sha256Digest, Satoshis])
      extends NetworkElement
      with MapWrapper[Sha256Digest, Satoshis] {
    override def wrapped: Map[Sha256Digest, Satoshis] = outcomeValueMap

    override def bytes: ByteVector = {
      outcomeValueMap.foldLeft(ByteVector.empty) {
        case (vec, (digest, sats)) => vec ++ digest.bytes ++ sats.bytes
      }
    }

    def toTLV: ContractInfoV0TLV = ContractInfoV0TLV(outcomeValueMap)
  }

  object ContractInfo extends Factory[ContractInfo] {

    private val sizeOfMapElement: Int = 40

    val empty: ContractInfo = ContractInfo(ByteVector.low(sizeOfMapElement))

    override def fromBytes(bytes: ByteVector): ContractInfo = {
      @tailrec
      def loop(
          remainingBytes: ByteVector,
          accum: Vector[(Sha256Digest, Satoshis)]): Vector[
        (Sha256Digest, Satoshis)] = {
        if (remainingBytes.size < sizeOfMapElement) {
          accum
        } else {
          val relevantBytes = remainingBytes.take(sizeOfMapElement)
          val digest = Sha256Digest(relevantBytes.take(32))
          val sats = Satoshis(relevantBytes.takeRight(8))
          loop(remainingBytes.drop(sizeOfMapElement), accum :+ (digest, sats))
        }
      }
      ContractInfo(loop(bytes, Vector.empty).toMap)
    }
  }

  sealed trait DLCSetupMessage extends DLCMessage {
    def pubKeys: DLCPublicKeys
    def totalCollateral: Satoshis
    def fundingInputs: Vector[OutputReference]
    def changeAddress: BitcoinAddress
    require(
      totalCollateral >= Satoshis.zero,
      s"Cannot have a negative totalCollateral, got: ${totalCollateral.toLong}")
  }

  /**
    * The initiating party starts the protocol by sending an offer message to the other party.
    *
    * @param contractInfo Contract information consists of a map to be used to create CETs
    * @param oracleInfo The oracle public key and R point(s) to use to build the CETs as
    *                   well as meta information to identify the oracle to be used in the contract.
    * @param pubKeys The relevant public keys that the initiator will be using
    * @param totalCollateral How much the initiator inputs into the contract.
    * @param fundingInputs The set of UTXOs to be used as input to the fund transaction.
    * @param changeAddress The address to use to send the change for the initiator.
    * @param feeRate The fee rate to be used when computing fees for the different transactions.
    * @param timeouts The set of timeouts for the CETs
    */
  case class DLCOffer(
      contractInfo: ContractInfo,
      oracleInfo: OracleInfo,
      pubKeys: DLCPublicKeys,
      totalCollateral: Satoshis,
      fundingInputs: Vector[OutputReference],
      changeAddress: BitcoinAddress,
      feeRate: SatoshisPerVirtualByte,
      timeouts: DLCTimeouts)
      extends DLCSetupMessage {

    val tempContractId: Sha256DigestBE =
      CryptoUtil.sha256(toTLV.bytes).flip

    def toTLV: DLCOfferTLV = {
      val chainHash =
        changeAddress.networkParameters.chainParams.genesisBlock.blockHeader.hashBE

      DLCOfferTLV(
        contractFlags = 0x00,
        chainHash = chainHash,
        contractInfo.toTLV,
        oracleInfo.toTLV,
        fundingPubKey = pubKeys.fundingKey,
        payoutSPK = pubKeys.payoutAddress.scriptPubKey,
        totalCollateralSatoshis = totalCollateral,
        fundingInputs = fundingInputs.map(FundingInputTempTLV(_)),
        changeSPK = changeAddress.scriptPubKey,
        feeRate = feeRate,
        contractMaturityBound = timeouts.contractMaturity,
        contractTimeout = timeouts.contractTimeout
      )
    }

    override def toJson: Value = {
      val contractInfosJson =
        contractInfo
          .map(info =>
            mutable.LinkedHashMap("sha256" -> Str(info._1.hex),
                                  "sats" -> Num(info._2.toLong.toDouble)))

      val fundingInputsJson =
        fundingInputs
          .map(input =>
            mutable.LinkedHashMap("outpoint" -> Str(input.outPoint.hex),
                                  "output" -> Str(input.output.hex)))

      val timeoutsJson =
        mutable.LinkedHashMap(
          "contractMaturity" -> Num(
            timeouts.contractMaturity.toUInt32.toLong.toDouble),
          "contractTimeout" -> Num(
            timeouts.contractTimeout.toUInt32.toLong.toDouble)
        )

      val pubKeysJson =
        mutable.LinkedHashMap(
          "fundingKey" -> Str(pubKeys.fundingKey.hex),
          "payoutAddress" -> Str(pubKeys.payoutAddress.value)
        )

      Obj(
        mutable.LinkedHashMap[String, Value](
          "contractInfo" -> contractInfosJson,
          "oracleInfo" -> Str(oracleInfo.hex),
          "pubKeys" -> pubKeysJson,
          "totalCollateral" -> Num(totalCollateral.toLong.toDouble),
          "fundingInputs" -> fundingInputsJson,
          "changeAddress" -> Str(changeAddress.value),
          "feeRate" -> Num(feeRate.toLong.toDouble),
          "timeouts" -> timeoutsJson
        )
      )
    }
  }

  object DLCOffer {

    def fromTLV(offer: DLCOfferTLV): DLCOffer = {
      val network = Networks.fromChainHash(offer.chainHash)

      val contractInfo = offer.contractInfo match {
        case ContractInfoV0TLV(outcomes) => ContractInfo(outcomes)
      }
      val oracleInfo = offer.oracleInfo match {
        case OracleInfoV0TLV(pubKey, rValue) => OracleInfo(pubKey, rValue)
      }

      DLCOffer(
        contractInfo = contractInfo,
        oracleInfo = oracleInfo,
        pubKeys = DLCPublicKeys(
          offer.fundingPubKey,
          BitcoinAddress.fromScriptPubKey(offer.payoutSPK, network)),
        totalCollateral = offer.totalCollateralSatoshis,
        fundingInputs = offer.fundingInputs.map {
          case FundingInputTempTLV(outputRef) => outputRef
        },
        changeAddress =
          BitcoinAddress.fromScriptPubKey(offer.changeSPK, network),
        feeRate = offer.feeRate,
        timeouts =
          DLCTimeouts(offer.contractMaturityBound, offer.contractTimeout)
      )
    }

    def fromJson(js: Value): DLCOffer = {
      val vec = js.obj.toVector

      val contractInfoMap =
        vec
          .find(_._1 == "contractInfo")
          .map {
            case (_, value) =>
              value.arr.map { subVal =>
                implicit val obj: mutable.LinkedHashMap[String, Value] =
                  subVal.obj

                val sha256 =
                  getValue("sha256")
                val sats = getValue("sats")

                (Sha256Digest(sha256.str), Satoshis(sats.num.toLong))
              }
          }
          .get
          .toMap

      val fundingInputs =
        vec
          .find(_._1 == "fundingInputs")
          .map {
            case (_, value) =>
              value.arr.map { subVal =>
                implicit val obj: mutable.LinkedHashMap[String, Value] =
                  subVal.obj

                val outpoint = getValue("outpoint")
                val output = getValue("output")

                OutputReference(TransactionOutPoint(outpoint.str),
                                TransactionOutput(output.str))
              }
          }
          .get
          .toVector

      val oracleInfo =
        vec.find(_._1 == "oracleInfo").map(obj => OracleInfo(obj._2.str)).get

      val pubKeys =
        vec
          .find(_._1 == "pubKeys")
          .map {
            case (_, value) =>
              implicit val obj: mutable.LinkedHashMap[String, Value] = value.obj

              val fundingKey = getValue("fundingKey")
              val payoutAddress = getValue("payoutAddress")

              DLCPublicKeys(
                ECPublicKey(fundingKey.str),
                BitcoinAddress(payoutAddress.str)
              )
          }
          .get

      val totalCollateral = vec
        .find(_._1 == "totalCollateral")
        .map(obj => Satoshis(obj._2.num.toLong))
        .get
      val changeAddress =
        vec
          .find(_._1 == "changeAddress")
          .map(obj => BitcoinAddress.fromString(obj._2.str))
          .get
      val feeRate =
        vec
          .find(_._1 == "feeRate")
          .map(obj => SatoshisPerVirtualByte(Satoshis(obj._2.num.toLong)))
          .get

      val timeouts =
        vec
          .find(_._1 == "timeouts")
          .map {
            case (_, value) =>
              implicit val obj: mutable.LinkedHashMap[String, Value] = value.obj
              val contractMaturity = getValue("contractMaturity")
              val contractTimeout = getValue("contractTimeout")

              DLCTimeouts(
                BlockTime(UInt32(contractMaturity.num.toLong)),
                BlockTime(UInt32(contractTimeout.num.toLong))
              )
          }
          .get

      DLCOffer(ContractInfo(contractInfoMap),
               oracleInfo,
               pubKeys,
               totalCollateral,
               fundingInputs,
               changeAddress,
               feeRate,
               timeouts)

    }
  }

  case class DLCAcceptWithoutSigs(
      totalCollateral: Satoshis,
      pubKeys: DLCPublicKeys,
      fundingInputs: Vector[OutputReference],
      changeAddress: BitcoinAddress,
      tempContractId: Sha256DigestBE) {

    def withSigs(cetSigs: CETSignatures): DLCAccept = {
      DLCAccept(totalCollateral = totalCollateral,
                pubKeys = pubKeys,
                fundingInputs = fundingInputs,
                changeAddress = changeAddress,
                cetSigs = cetSigs,
                tempContractId = tempContractId)
    }
  }

  case class DLCAccept(
      totalCollateral: Satoshis,
      pubKeys: DLCPublicKeys,
      fundingInputs: Vector[OutputReference],
      changeAddress: BitcoinAddress,
      cetSigs: CETSignatures,
      tempContractId: Sha256DigestBE)
      extends DLCSetupMessage {

    def toTLV: DLCAcceptTLV = {
      DLCAcceptTLV(
        tempContractId = tempContractId,
        totalCollateralSatoshis = totalCollateral,
        fundingPubKey = pubKeys.fundingKey,
        payoutSPK = pubKeys.payoutAddress.scriptPubKey,
        fundingInputs = fundingInputs.map(FundingInputTempTLV(_)),
        changeSPK = changeAddress.scriptPubKey,
        cetSignatures = CETSignaturesV0TLV(cetSigs.outcomeSigs.values.toVector),
        refundSignature = cetSigs.refundSig.signature
      )
    }

    def toJson: Value = {
      val fundingInputsJson =
        fundingInputs.map(input =>
          mutable.LinkedHashMap("outpoint" -> Str(input.outPoint.hex),
                                "output" -> Str(input.output.hex)))

      val outcomeSigsJson =
        cetSigs.outcomeSigs.map {
          case (hash, sig) =>
            mutable.LinkedHashMap(hash.hex -> Str(sig.hex))
        }

      val cetSigsJson =
        mutable.LinkedHashMap("outcomeSigs" -> Value(outcomeSigsJson),
                              "refundSig" -> Str(cetSigs.refundSig.hex))
      val pubKeysJson =
        mutable.LinkedHashMap(
          "fundingKey" -> Str(pubKeys.fundingKey.hex),
          "payoutAddress" -> Str(pubKeys.payoutAddress.value)
        )

      Obj(
        mutable.LinkedHashMap[String, Value](
          "totalCollateral" -> Num(totalCollateral.toLong.toDouble),
          "pubKeys" -> pubKeysJson,
          "fundingInputs" -> fundingInputsJson,
          "changeAddress" -> Str(changeAddress.value),
          "cetSigs" -> cetSigsJson,
          "tempContractId" -> Str(tempContractId.hex)
        )
      )
    }

    def withoutSigs: DLCAcceptWithoutSigs = {
      DLCAcceptWithoutSigs(totalCollateral,
                           pubKeys,
                           fundingInputs,
                           changeAddress,
                           tempContractId)
    }
  }

  object DLCAccept {

    def fromTLV(
        accept: DLCAcceptTLV,
        network: NetworkParameters,
        outcomes: Vector[Sha256Digest]): DLCAccept = {
      val outcomeSigs = accept.cetSignatures match {
        case CETSignaturesV0TLV(sigs) =>
          outcomes.zip(sigs).toMap
      }

      DLCAccept(
        totalCollateral = accept.totalCollateralSatoshis,
        pubKeys = DLCPublicKeys(
          accept.fundingPubKey,
          BitcoinAddress.fromScriptPubKey(accept.payoutSPK, network)),
        fundingInputs = accept.fundingInputs.map {
          case FundingInputTempTLV(outputRef) => outputRef
        },
        changeAddress =
          BitcoinAddress.fromScriptPubKey(accept.changeSPK, network),
        cetSigs = CETSignatures(
          outcomeSigs,
          PartialSignature(accept.fundingPubKey, accept.refundSignature)),
        tempContractId = accept.tempContractId
      )
    }

    def fromTLV(accept: DLCAcceptTLV, offer: DLCOffer): DLCAccept = {
      fromTLV(accept,
              offer.changeAddress.networkParameters,
              offer.contractInfo.outcomeValueMap.keys.toVector)
    }

    def fromJson(js: Value): DLCAccept = {
      val vec = js.obj.toVector

      val totalCollateral = vec
        .find(_._1 == "totalCollateral")
        .map(obj => Satoshis(obj._2.num.toLong))
        .get
      val changeAddress =
        vec
          .find(_._1 == "changeAddress")
          .map(obj => BitcoinAddress.fromString(obj._2.str))
          .get

      val pubKeys =
        vec
          .find(_._1 == "pubKeys")
          .map {
            case (_, value) =>
              implicit val obj: mutable.LinkedHashMap[String, Value] = value.obj

              val fundingKey =
                getValue("fundingKey")
              val payoutAddress =
                getValue("payoutAddress")

              DLCPublicKeys(
                ECPublicKey(fundingKey.str),
                BitcoinAddress(payoutAddress.str)
              )
          }
          .get

      val fundingInputs =
        vec
          .find(_._1 == "fundingInputs")
          .map {
            case (_, value) =>
              value.arr.map { subVal =>
                implicit val obj: mutable.LinkedHashMap[String, Value] =
                  subVal.obj

                val outpoint = getValue("outpoint")
                val output = getValue("output")

                OutputReference(TransactionOutPoint(outpoint.str),
                                TransactionOutput(output.str))
              }
          }
          .get
          .toVector

      val cetSigs =
        vec
          .find(_._1 == "cetSigs")
          .map {
            case (_, value) =>
              implicit val obj: mutable.LinkedHashMap[String, Value] = value.obj

              val outcomeSigsMap = getValue("outcomeSigs")
              val outcomeSigs = outcomeSigsMap.arr.map { v =>
                val (key, value) = v.obj.head
                val hash = Sha256Digest(key)
                val sig = ECAdaptorSignature(value.str)
                (hash, sig)
              }

              val refundSig = getValue("refundSig")

              CETSignatures(
                outcomeSigs.toMap,
                PartialSignature(refundSig.str)
              )
          }
          .get

      val tempContractId =
        vec
          .find(_._1 == "tempContractId")
          .map(obj => Sha256DigestBE(obj._2.str))
          .get

      DLCAccept(totalCollateral,
                pubKeys,
                fundingInputs,
                changeAddress,
                cetSigs,
                tempContractId)
    }
  }

  case class DLCSign(
      cetSigs: CETSignatures,
      fundingSigs: FundingSignatures,
      contractId: ByteVector)
      extends DLCMessage {

    def toTLV: DLCSignTLV = {
      DLCSignTLV(
        contractId = contractId,
        cetSignatures = CETSignaturesV0TLV(cetSigs.outcomeSigs.values.toVector),
        refundSignature = cetSigs.refundSig.signature,
        fundingSignatures = fundingSigs.toTLV
      )
    }

    def toJson: Value = {

      val fundingSigsMap = fundingSigs.map(outPointAndSig =>
        (outPointAndSig._1.hex,
         Arr.from(outPointAndSig._2.map(sig => Str(sig.hex)))))

      val fundingSigsJson = fundingSigsMap
        .foldLeft(mutable.LinkedHashMap.newBuilder[String, Value])(
          (builder, element) => builder += element)
        .result()

      val outcomeSigsJson =
        cetSigs.outcomeSigs.map {
          case (hash, sig) =>
            mutable.LinkedHashMap(hash.hex -> Str(sig.hex))
        }

      val cetSigsJson =
        mutable.LinkedHashMap("outcomeSigs" -> Value(outcomeSigsJson),
                              "refundSig" -> Str(cetSigs.refundSig.hex))

      Obj(
        mutable.LinkedHashMap[String, Value](
          "cetSigs" -> cetSigsJson,
          "fundingSigs" -> fundingSigsJson,
          "contractId" -> Str(contractId.toHex)
        )
      )
    }
  }

  object DLCSign {

    def fromTLV(
        sign: DLCSignTLV,
        fundingPubKey: ECPublicKey,
        outcomes: Vector[Sha256Digest],
        inputPubKeys: Map[TransactionOutPoint, ECPublicKey]): DLCSign = {
      val outcomeSigs = sign.cetSignatures match {
        case CETSignaturesV0TLV(sigs) =>
          outcomes.zip(sigs).toMap
      }

      val fundingSigMap = sign.fundingSignatures match {
        case FundingSignaturesV0TLV(sigs) =>
          sigs.map {
            case (outPoint, sig) =>
              outPoint -> Vector(PartialSignature(inputPubKeys(outPoint), sig))
          }
      }

      DLCSign(
        cetSigs =
          CETSignatures(outcomeSigs,
                        PartialSignature(fundingPubKey, sign.refundSignature)),
        fundingSigs = FundingSignatures(fundingSigMap),
        contractId = sign.contractId
      )
    }

    def fromTLV(
        sign: DLCSignTLV,
        offer: DLCOffer,
        inputPubKeys: Map[TransactionOutPoint, ECPublicKey]): DLCSign = {
      fromTLV(sign,
              offer.pubKeys.fundingKey,
              offer.contractInfo.outcomeValueMap.keys.toVector,
              inputPubKeys)
    }

    def fromJson(js: Value): DLCSign = {
      val vec = js.obj.toVector

      val cetSigs =
        vec
          .find(_._1 == "cetSigs")
          .map {
            case (_, value) =>
              implicit val obj: mutable.LinkedHashMap[String, Value] = value.obj

              val outcomeSigsMap = getValue("outcomeSigs")
              val outcomeSigs = outcomeSigsMap.arr.map { item =>
                val (key, value) = item.obj.head
                val hash = Sha256Digest(key)
                val sig = ECAdaptorSignature(value.str)
                (hash, sig)
              }

              val refundSig = getValue("refundSig")

              CETSignatures(
                outcomeSigs.toMap,
                PartialSignature(refundSig.str)
              )
          }
          .get

      val fundingSigs =
        vec
          .find(_._1 == "fundingSigs")
          .map {
            case (_, value) =>
              if (value.obj.isEmpty) {
                throw new RuntimeException(
                  s"DLC Sign cannot have empty fundingSigs, got $js")
              } else {
                value.obj.toVector.map {
                  case (outPoint, sigs) =>
                    (TransactionOutPoint(outPoint),
                     sigs.arr
                       .map(sig => PartialSignature(sig.str))
                       .toVector)
                }
              }
          }
          .get
          .toMap

      val contractId =
        vec
          .find(_._1 == "contractId")
          .map(obj => ByteVector.fromValidHex(obj._2.str))
          .get

      DLCSign(cetSigs, FundingSignatures(fundingSigs), contractId)
    }
  }
}
