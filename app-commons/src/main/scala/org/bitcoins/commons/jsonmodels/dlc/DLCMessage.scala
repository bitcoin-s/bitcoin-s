package org.bitcoins.commons.jsonmodels.dlc

import java.nio.charset.StandardCharsets

import org.bitcoins.core.config.{NetworkParameters, Networks}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{UInt16, UInt32}
import org.bitcoins.core.protocol.script.{ScriptWitnessV0, WitnessScriptPubKey}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.protocol.{BigSizeUInt, BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.serializers.script.RawScriptWitnessParser
import org.bitcoins.core.util.SeqWrapper
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

  def calcParamHash(
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts): Sha256DigestBE = {
    CryptoUtil
      .sha256(oracleInfo.bytes ++ contractInfo.bytes ++ timeouts.bytes)
      .flip
  }

  private def getValue(key: String)(implicit
      obj: mutable.LinkedHashMap[String, Value]): Value = {
    val index = obj.keys.toList.indexOf(key)
    obj.values(index)
  }

  sealed trait OracleInfo extends NetworkElement {
    def pubKey: SchnorrPublicKey
    def nonces: Vector[SchnorrNonce]

    def verifySigs(
        outcome: DLCOutcomeType,
        sigs: Vector[SchnorrDigitalSignature]): Boolean

    def toTLV: OracleInfoTLV

    def sigPoint(outcome: DLCOutcomeType): ECPublicKey = {
      outcome.serialized
        .zip(nonces)
        .map {
          case (bytes, nonce) =>
            pubKey.computeSigPoint(CryptoUtil.sha256(bytes).bytes, nonce)
        }
        .reduce(_.add(_))
    }
  }

  case class SingleNonceOracleInfo(
      pubKey: SchnorrPublicKey,
      rValue: SchnorrNonce)
      extends OracleInfo {
    override def nonces: Vector[SchnorrNonce] = Vector(rValue)

    override def verifySigs(
        outcome: DLCOutcomeType,
        sigs: Vector[SchnorrDigitalSignature]): Boolean = {
      outcome match {
        case EnumOutcome(outcome) =>
          if (sigs.length != 1) {
            throw new IllegalArgumentException(
              s"Expected one signature, got $sigs")
          } else if (sigs.head.rx != rValue) {
            throw new IllegalArgumentException(
              s"Expected R value of $rValue, got ${sigs.head}")
          } else {
            sigs.length == 1 && pubKey.verify(CryptoUtil.sha256(outcome).bytes,
                                              sigs.head)
          }
        case UnsignedNumericOutcome(_) =>
          throw new IllegalArgumentException(
            s"Expected EnumOutcome, got $outcome")
      }
    }

    override def bytes: ByteVector = pubKey.bytes ++ rValue.bytes

    override def toTLV: OracleInfoV0TLV = OracleInfoV0TLV(pubKey, rValue)
  }

  object SingleNonceOracleInfo extends Factory[SingleNonceOracleInfo] {

    override def fromBytes(bytes: ByteVector): SingleNonceOracleInfo = {
      require(bytes.size == 64, s"OracleInfo is only 64 bytes, got $bytes")

      val pubkey = SchnorrPublicKey(bytes.take(32))
      val rValue = SchnorrNonce(bytes.drop(32))

      SingleNonceOracleInfo(pubkey, rValue)
    }
  }

  case class MultiNonceOracleInfo(
      pubKey: SchnorrPublicKey,
      nonces: Vector[SchnorrNonce])
      extends OracleInfo {

    override def verifySigs(
        outcome: DLCOutcomeType,
        sigs: Vector[SchnorrDigitalSignature]): Boolean = {
      require(
        sigs.length <= nonces.length,
        s"Too many signatures (expected at most ${nonces.length}), got $sigs")

      outcome match {
        case EnumOutcome(_) =>
          throw new IllegalArgumentException(
            s"Expected numeric outcome, got $outcome")
        case UnsignedNumericOutcome(digits) =>
          digits
            .zip(sigs.take(digits.length).zip(nonces.take(digits.length)))
            .foldLeft(digits.length <= sigs.length) {
              case (result, (digit, (sig, nonce))) =>
                require(sig.rx == nonce,
                        s"Unexpected nonce in $sig, expected $nonce")

                result && pubKey.verify(CryptoUtil.sha256(digit.toString).bytes,
                                        sig)
            }
      }
    }

    override def bytes: ByteVector =
      pubKey.bytes ++ nonces.foldLeft(ByteVector.empty)(_ ++ _.bytes)

    override def toTLV: OracleInfoV1TLV = OracleInfoV1TLV(pubKey, nonces)
  }

  object MultiNonceOracleInfo extends Factory[MultiNonceOracleInfo] {

    override def fromBytes(bytes: ByteVector): MultiNonceOracleInfo = {
      require(bytes.length > 32 && bytes.length % 32 == 0,
              s"Invalid MultiNonceOracleInfo bytes: $bytes")

      @tailrec
      def loop(
          bytesLeft: ByteVector,
          nonces: Vector[SchnorrNonce]): Vector[SchnorrNonce] = {
        if (bytesLeft.isEmpty) {
          nonces
        } else {
          val (nonceBytes, newBytesLeft) = bytesLeft.splitAt(32)
          loop(newBytesLeft, nonces :+ SchnorrNonce(nonceBytes))
        }
      }

      val pubKey = SchnorrPublicKey(bytes.take(32))
      val nonces = loop(bytes.drop(32), Vector.empty)

      MultiNonceOracleInfo(pubKey, nonces)
    }
  }

  object OracleInfo extends Factory[OracleInfo] {

    val dummy: OracleInfo = SingleNonceOracleInfo(ByteVector.fill(64)(1))

    override def fromBytes(bytes: ByteVector): OracleInfo = {
      if (bytes.length > 64) {
        MultiNonceOracleInfo(bytes)
      } else {
        SingleNonceOracleInfo(bytes)
      }
    }

    def fromTLV(tlv: OracleInfoTLV): OracleInfo = {
      tlv match {
        case OracleInfoV0TLV(pubKey, rValue) =>
          SingleNonceOracleInfo(pubKey, rValue)
        case OracleInfoV1TLV(pubKey, nonces) =>
          MultiNonceOracleInfo(pubKey, nonces)
      }
    }
  }

  sealed trait ContractInfo extends NetworkElement {

    /** Returns the counter-party's ContractInfo corresponding to this one */
    def flip(totalCollateral: Satoshis): ContractInfo
    def toTLV: ContractInfoTLV
    def allOutcomes: Vector[DLCOutcomeType]
    def apply(outcome: DLCOutcomeType): Satoshis

    /** Returns the maximum payout this party could win from this contract */
    def max: Satoshis
  }

  case class SingleNonceContractInfo(
      outcomeValueMap: Vector[(EnumOutcome, Satoshis)])
      extends ContractInfo
      with SeqWrapper[(EnumOutcome, Satoshis)] {

    override def apply(outcome: DLCOutcomeType): Satoshis = {
      outcome match {
        case outcome: EnumOutcome =>
          outcomeValueMap
            .find(_._1 == outcome)
            .map(_._2)
            .getOrElse(throw new IllegalArgumentException(
              s"No value found for key $outcome"))
        case UnsignedNumericOutcome(_) =>
          throw new IllegalArgumentException(s"Expected EnumOutcome: $outcome")
      }
    }

    override def wrapped: Vector[(EnumOutcome, Satoshis)] = outcomeValueMap

    def keys: Vector[EnumOutcome] = outcomeValueMap.map(_._1)

    def values: Vector[Satoshis] = outcomeValueMap.map(_._2)

    override def allOutcomes: Vector[DLCOutcomeType] = keys

    override def max: Satoshis = values.maxBy(_.toLong)

    override def bytes: ByteVector = {
      outcomeValueMap.foldLeft(ByteVector.empty) {
        case (vec, (str, sats)) =>
          val strBytes = CryptoUtil.serializeForHash(str.outcome)
          vec ++ BigSizeUInt.calcFor(strBytes).bytes ++ strBytes ++ sats.bytes
      }
    }

    override def toTLV: ContractInfoV0TLV =
      ContractInfoV0TLV(outcomeValueMap.map {
        case (outcome, amt) => outcome.outcome -> amt
      })

    override def flip(totalCollateral: Satoshis): SingleNonceContractInfo = {
      SingleNonceContractInfo(outcomeValueMap.map {
        case (hash, amt) => (hash, (totalCollateral - amt).satoshis)
      })
    }
  }

  object SingleNonceContractInfo extends Factory[SingleNonceContractInfo] {

    def fromStringVec(
        vec: Vector[(String, Satoshis)]): SingleNonceContractInfo = {
      SingleNonceContractInfo(vec.map {
        case (str, amt) => EnumOutcome(str) -> amt
      })
    }

    override def fromBytes(bytes: ByteVector): SingleNonceContractInfo = {
      @tailrec
      def loop(
          remainingBytes: ByteVector,
          accum: Vector[(EnumOutcome, Satoshis)]): Vector[
        (EnumOutcome, Satoshis)] = {
        if (remainingBytes.isEmpty) {
          accum
        } else {
          val outcomeSize = BigSizeUInt(remainingBytes)
          val outcome =
            remainingBytes.drop(outcomeSize.byteSize).take(outcomeSize.toInt)
          val outcomeStr = new String(outcome.toArray, StandardCharsets.UTF_8)
          val outcomeSizeAndOutcomeLen =
            outcomeSize.byteSize + outcomeSize.toInt
          val sats = Satoshis(
            remainingBytes.drop(outcomeSizeAndOutcomeLen).take(8))

          loop(remainingBytes.drop(outcomeSizeAndOutcomeLen + 8),
               accum :+ (EnumOutcome(outcomeStr), sats))
        }
      }

      SingleNonceContractInfo(loop(bytes, Vector.empty))
    }
  }

  /** Contains a deterministically compressed set of outcomes computed from
    * a given payout curve.
    */
  case class MultiNonceContractInfo(
      outcomeValueFunc: OutcomeValueFunction,
      base: Int,
      numDigits: Int,
      totalCollateral: Satoshis)
      extends ContractInfo {

    lazy val outcomeVec: Vector[(Vector[Int], Satoshis)] =
      CETCalculator.computeCETs(base,
                                numDigits,
                                outcomeValueFunc,
                                totalCollateral,
                                RoundingIntervals.noRounding)

    override def apply(outcome: DLCOutcomeType): Satoshis = {
      outcome match {
        case UnsignedNumericOutcome(digits) =>
          outcomeVec.find {
            case (possibleOutcome, _) => digits.startsWith(possibleOutcome)
          } match {
            case Some((_, amt)) => amt
            case None =>
              throw new IllegalArgumentException(
                s"Unrecognized outcome: $digits")
          }
        case EnumOutcome(_) =>
          throw new IllegalArgumentException(
            s"Expected UnsignedNumericOutcome: $outcome")
      }
    }

    override def allOutcomes: Vector[DLCOutcomeType] =
      outcomeVec.map { case (outcome, _) => UnsignedNumericOutcome(outcome) }

    override def max: Satoshis = totalCollateral

    override def flip(totalCollateral: Satoshis): MultiNonceContractInfo = {
      require(
        totalCollateral == this.totalCollateral,
        s"Input total collateral ($totalCollateral) did not match ${this.totalCollateral}")

      val flippedFunc = OutcomeValueFunction(outcomeValueFunc.points.map {
        point => point.copy(value = (totalCollateral - point.value).satoshis)
      })

      MultiNonceContractInfo(
        flippedFunc,
        base,
        numDigits,
        totalCollateral
      )
    }

    override def bytes: ByteVector = ByteVector.empty // TODO: Fix this

    override def toTLV: ContractInfoV1TLV = {
      val tlvPoints = outcomeValueFunc.points.map { point =>
        TLVPoint(point.outcome.toLongExact, point.value, point.isEndpoint)
      }

      ContractInfoV1TLV(base, numDigits, totalCollateral, tlvPoints)
    }
  }

  object ContractInfo extends Factory[ContractInfo] {

    val empty: ContractInfo = SingleNonceContractInfo(
      Vector(EnumOutcome("") -> Satoshis.zero))

    override def fromBytes(bytes: ByteVector): ContractInfo = {
      SingleNonceContractInfo.fromBytes(bytes)
    }

    def fromTLV(tlv: ContractInfoTLV): ContractInfo = {
      tlv match {
        case ContractInfoV0TLV(outcomes) =>
          SingleNonceContractInfo.fromStringVec(outcomes)
        case ContractInfoV1TLV(base, numDigits, totalCollateral, tlvPoints) =>
          val points = tlvPoints.map { point =>
            OutcomeValuePoint(point.outcome, point.value, point.isEndpoint)
          }

          MultiNonceContractInfo(OutcomeValueFunction(points),
                                 base,
                                 numDigits,
                                 totalCollateral)
      }
    }
  }

  case class OracleAndContractInfo(
      oracleInfo: OracleInfo,
      offerContractInfo: ContractInfo,
      acceptContractInfo: ContractInfo) {
    (oracleInfo, offerContractInfo, acceptContractInfo) match {
      case (_: SingleNonceOracleInfo,
            _: SingleNonceContractInfo,
            _: SingleNonceContractInfo) =>
        ()
      case (_: MultiNonceOracleInfo,
            info1: MultiNonceContractInfo,
            info2: MultiNonceContractInfo) =>
        require(
          info1.totalCollateral == info2.totalCollateral,
          s"Contract Infos must have matching total collateral, got $offerContractInfo - $acceptContractInfo"
        )
      case (_: OracleInfo, _: ContractInfo, _: ContractInfo) =>
        throw new IllegalArgumentException(
          s"All infos must be for the same kind of outcome: $this")
    }

    def verifySigs(
        outcome: DLCOutcomeType,
        sigs: Vector[SchnorrDigitalSignature]): Boolean = {
      oracleInfo.verifySigs(outcome, sigs)
    }

    lazy val outcomeMap: Map[
      DLCOutcomeType,
      (ECPublicKey, Satoshis, Satoshis)] =
      allOutcomes.map { msg =>
        msg -> (oracleInfo.sigPoint(msg), offerContractInfo(
          msg), acceptContractInfo(msg))
      }.toMap

    def resultOfOutcome(
        outcome: DLCOutcomeType): (ECPublicKey, Satoshis, Satoshis) = {
      outcomeMap(outcome)
    }

    def outcomeFromSignature(
        sigs: Vector[SchnorrDigitalSignature]): DLCOutcomeType = {
      val sigPoint = sigs.map(_.sig.getPublicKey).reduce(_.add(_))
      outcomeMap.find(_._2._1 == sigPoint) match {
        case Some((outcome, _)) => outcome
        case None =>
          throw new IllegalArgumentException(
            s"Signatures do not correspond to a possible outcome! $sigs")
      }
    }

    def sigPointForOutcome(outcome: DLCOutcomeType): ECPublicKey = {
      resultOfOutcome(outcome)._1
    }

    lazy val allOutcomes: Vector[DLCOutcomeType] =
      offerContractInfo.allOutcomes

    /** Returns the payouts for the signature as (toOffer, toAccept) */
    def getPayouts(
        sigs: Vector[SchnorrDigitalSignature]): (Satoshis, Satoshis) = {
      val outcome = outcomeFromSignature(sigs)
      getPayouts(outcome)
    }

    /** Returns the payouts for the outcome as (toOffer, toAccept) */
    def getPayouts(outcome: DLCOutcomeType): (Satoshis, Satoshis) = {
      val (_, offerOutcome, acceptOutcome) = resultOfOutcome(outcome)

      (offerOutcome, acceptOutcome)
    }
  }

  object OracleAndContractInfo {

    def apply(
        oracleInfo: OracleInfo,
        offerContractInfo: ContractInfo): OracleAndContractInfo = {
      OracleAndContractInfo(oracleInfo,
                            offerContractInfo,
                            offerContractInfo.flip(offerContractInfo.max))
    }
  }

  sealed trait DLCSetupMessage extends DLCMessage {
    def pubKeys: DLCPublicKeys

    def totalCollateral: Satoshis

    def fundingInputs: Vector[DLCFundingInput]

    def changeAddress: BitcoinAddress

    require(
      totalCollateral >= Satoshis.zero,
      s"Cannot have a negative totalCollateral, got: ${totalCollateral.toLong}")
  }

  /**
    * The initiating party starts the protocol by sending an offer message to the other party.
    *
    * @param oracleAndContractInfo The oracle public key and R point(s) to use to build the CETs as
    *                   well as meta information to identify the oracle to be used in the contract,
    *                   and a map to be used to create CETs.
    * @param pubKeys The relevant public keys that the initiator will be using
    * @param totalCollateral How much the initiator inputs into the contract.
    * @param fundingInputs   The set of UTXOs to be used as input to the fund transaction.
    * @param changeAddress   The address to use to send the change for the initiator.
    * @param feeRate         The fee rate to be used when computing fees for the different transactions.
    * @param timeouts        The set of timeouts for the CETs
    */
  case class DLCOffer(
      oracleAndContractInfo: OracleAndContractInfo,
      pubKeys: DLCPublicKeys,
      totalCollateral: Satoshis,
      fundingInputs: Vector[DLCFundingInput],
      changeAddress: BitcoinAddress,
      feeRate: SatoshisPerVirtualByte,
      timeouts: DLCTimeouts)
      extends DLCSetupMessage {

    val oracleInfo: OracleInfo = oracleAndContractInfo.oracleInfo
    val contractInfo: ContractInfo = oracleAndContractInfo.offerContractInfo

    lazy val paramHash: Sha256DigestBE =
      calcParamHash(oracleInfo, contractInfo, timeouts)

    val tempContractId: Sha256Digest =
      CryptoUtil.sha256(toMessage.bytes)

    def toTLV: DLCOfferTLV = {
      val chainHash =
        changeAddress.networkParameters.chainParams.genesisBlock.blockHeader.hash

      DLCOfferTLV(
        contractFlags = 0x00,
        chainHash = chainHash,
        contractInfo.toTLV,
        oracleInfo.toTLV,
        fundingPubKey = pubKeys.fundingKey,
        payoutSPK = pubKeys.payoutAddress.scriptPubKey,
        totalCollateralSatoshis = totalCollateral,
        fundingInputs = fundingInputs.map(_.toTLV),
        changeSPK = changeAddress.scriptPubKey,
        feeRate = feeRate,
        contractMaturityBound = timeouts.contractMaturity,
        contractTimeout = timeouts.contractTimeout
      )
    }

    def toMessage: LnMessage[DLCOfferTLV] = {
      LnMessage(this.toTLV)
    }

    override def toJson: Value = {
      val contractInfosJson =
        contractInfo match {
          case SingleNonceContractInfo(outcomeValueMap) =>
            outcomeValueMap.map(info =>
              mutable.LinkedHashMap("outcome" -> Str(info._1.outcome),
                                    "sats" -> Num(info._2.toLong.toDouble)))
          case MultiNonceContractInfo(_, _, _, _) => ???
        }

      val fundingInputsJson =
        fundingInputs.map { input =>
          val obj = mutable.LinkedHashMap(
            "prevTx" -> Str(input.prevTx.hex),
            "prevTxVout" -> Num(input.prevTxVout.toLong.toDouble),
            "sequence" -> Num(input.sequence.toLong.toDouble),
            "maxWitnessLength" -> Num(input.maxWitnessLen.toLong.toDouble)
          )

          input.redeemScriptOpt.foreach { redeemScript =>
            obj.+=("redeemScript" -> Str(redeemScript.hex))
          }

          obj
        }

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
      val network = Networks.fromChainHash(offer.chainHash.flip)

      val contractInfo = ContractInfo.fromTLV(offer.contractInfo)
      val oracleInfo = OracleInfo.fromTLV(offer.oracleInfo)

      DLCOffer(
        oracleAndContractInfo = OracleAndContractInfo(oracleInfo, contractInfo),
        pubKeys = DLCPublicKeys(
          offer.fundingPubKey,
          BitcoinAddress.fromScriptPubKey(offer.payoutSPK, network)),
        totalCollateral = offer.totalCollateralSatoshis,
        fundingInputs = offer.fundingInputs.map {
          case input: FundingInputV0TLV => DLCFundingInput.fromTLV(input)
        },
        changeAddress =
          BitcoinAddress.fromScriptPubKey(offer.changeSPK, network),
        feeRate = offer.feeRate,
        timeouts =
          DLCTimeouts(offer.contractMaturityBound, offer.contractTimeout)
      )
    }

    def fromMessage(offer: LnMessage[DLCOfferTLV]): DLCOffer = {
      fromTLV(offer.tlv)
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

                val outcome =
                  getValue("outcome")
                val sats = getValue("sats")

                (EnumOutcome(outcome.str), Satoshis(sats.num.toLong))
              }
          }
          .get
          .toVector

      val fundingInputs =
        vec
          .find(_._1 == "fundingInputs")
          .map {
            case (_, value) =>
              value.arr.map { subVal =>
                implicit val obj: mutable.LinkedHashMap[String, Value] =
                  subVal.obj

                val prevTx = Transaction(getValue("prevTx").str)
                val prevTxVout = UInt32(getValue("prevTxVout").num.toInt)
                val sequence = UInt32(getValue("sequence").num.toLong)
                val maxWitnessLen =
                  UInt16(getValue("maxWitnessLength").num.toInt)
                val redeemScriptOpt = obj.find(_._1 == "redeemScript").map {
                  case (_, redeemScript) =>
                    WitnessScriptPubKey(redeemScript.str)
                }

                DLCFundingInput(prevTx,
                                prevTxVout,
                                sequence,
                                maxWitnessLen,
                                redeemScriptOpt)
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
                BlockTimeStamp(UInt32(contractMaturity.num.toLong)),
                BlockTimeStamp(UInt32(contractTimeout.num.toLong))
              )
          }
          .get

      DLCOffer(OracleAndContractInfo(oracleInfo,
                                     SingleNonceContractInfo(contractInfoMap)),
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
      fundingInputs: Vector[DLCFundingInput],
      changeAddress: BitcoinAddress,
      tempContractId: Sha256Digest) {

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
      fundingInputs: Vector[DLCFundingInput],
      changeAddress: BitcoinAddress,
      cetSigs: CETSignatures,
      tempContractId: Sha256Digest)
      extends DLCSetupMessage {

    def toTLV: DLCAcceptTLV = {
      DLCAcceptTLV(
        tempContractId = tempContractId,
        totalCollateralSatoshis = totalCollateral,
        fundingPubKey = pubKeys.fundingKey,
        payoutSPK = pubKeys.payoutAddress.scriptPubKey,
        fundingInputs = fundingInputs.map(_.toTLV),
        changeSPK = changeAddress.scriptPubKey,
        cetSignatures = CETSignaturesV0TLV(cetSigs.adaptorSigs),
        refundSignature =
          ECDigitalSignature.fromFrontOfBytes(cetSigs.refundSig.signature.bytes)
      )
    }

    def toMessage: LnMessage[DLCAcceptTLV] = {
      LnMessage(this.toTLV)
    }

    def toJson: Value = {
      val fundingInputsJson =
        fundingInputs.map { input =>
          val obj = mutable.LinkedHashMap(
            "prevTx" -> Str(input.prevTx.hex),
            "prevTxVout" -> Num(input.prevTxVout.toInt),
            "sequence" -> Num(input.sequence.toLong.toDouble),
            "maxWitnessLength" -> Num(input.maxWitnessLen.toInt)
          )

          input.redeemScriptOpt.foreach { redeemScript =>
            obj.+=("redeemScript" -> Str(redeemScript.hex))
          }

          obj
        }

      val outcomeSigsJson =
        cetSigs.outcomeSigs.map {
          case (outcome, sig) =>
            val str = outcome match {
              case EnumOutcome(outcome)      => outcome
              case UnsignedNumericOutcome(_) => ???
            }
            mutable.LinkedHashMap(str -> Str(sig.hex))
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
        outcomes: Vector[DLCOutcomeType]): DLCAccept = {
      val outcomeSigs = accept.cetSignatures match {
        case CETSignaturesV0TLV(sigs) =>
          outcomes.zip(sigs)
      }

      DLCAccept(
        totalCollateral = accept.totalCollateralSatoshis,
        pubKeys = DLCPublicKeys(
          accept.fundingPubKey,
          BitcoinAddress.fromScriptPubKey(accept.payoutSPK, network)),
        fundingInputs = accept.fundingInputs.map {
          case input: FundingInputV0TLV => DLCFundingInput.fromTLV(input)
        },
        changeAddress =
          BitcoinAddress.fromScriptPubKey(accept.changeSPK, network),
        cetSigs = CETSignatures(
          outcomeSigs,
          PartialSignature(
            accept.fundingPubKey,
            ECDigitalSignature(
              accept.refundSignature.bytes :+ HashType.sigHashAll.byte))),
        tempContractId = accept.tempContractId
      )
    }

    def fromTLV(accept: DLCAcceptTLV, offer: DLCOffer): DLCAccept = {
      offer.contractInfo match {
        case info: SingleNonceContractInfo =>
          fromTLV(accept, offer.changeAddress.networkParameters, info.keys)
        case MultiNonceContractInfo(_, _, _, _) => ???
      }
    }

    def fromMessage(
        accept: LnMessage[DLCAcceptTLV],
        offer: DLCOffer): DLCAccept = {
      fromTLV(accept.tlv, offer)
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

                val prevTx = Transaction(getValue("prevTx").str)
                val prevTxVout = UInt32(getValue("prevTxVout").num.toInt)
                val sequence = UInt32(getValue("sequence").num.toLong)
                val maxWitnessLen =
                  UInt16(getValue("maxWitnessLength").num.toInt)
                val redeemScriptOpt = obj.find(_._1 == "redeemScript").map {
                  case (_, redeemScript) =>
                    WitnessScriptPubKey(redeemScript.str)
                }

                DLCFundingInput(prevTx,
                                prevTxVout,
                                sequence,
                                maxWitnessLen,
                                redeemScriptOpt)
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
                val sig = ECAdaptorSignature(value.str)
                (EnumOutcome(key), sig)
              }

              val refundSig = getValue("refundSig")

              CETSignatures(
                outcomeSigs.toVector,
                PartialSignature(refundSig.str)
              )
          }
          .get

      val tempContractId =
        vec
          .find(_._1 == "tempContractId")
          .map(obj => Sha256Digest(obj._2.str))
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
        cetSignatures = CETSignaturesV0TLV(cetSigs.adaptorSigs),
        refundSignature = ECDigitalSignature.fromFrontOfBytes(
          cetSigs.refundSig.signature.bytes),
        fundingSignatures = fundingSigs.toTLV
      )
    }

    def toMessage: LnMessage[DLCSignTLV] = {
      LnMessage(this.toTLV)
    }

    def toJson: Value = {

      val fundingSigsMap = fundingSigs.map {
        case (outPoint, scriptWitness) =>
          (outPoint.hex, Str(scriptWitness.hex))
      }

      val fundingSigsJson = fundingSigsMap
        .foldLeft(mutable.LinkedHashMap.newBuilder[String, Value])(
          (builder, element) => builder += element)
        .result()

      val outcomeSigsJson =
        cetSigs.outcomeSigs.map {
          case (outcome, sig) =>
            val str = outcome match {
              case EnumOutcome(outcome)      => outcome
              case UnsignedNumericOutcome(_) => ???
            }
            mutable.LinkedHashMap(str -> Str(sig.hex))
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
        outcomes: Vector[DLCOutcomeType],
        fundingOutPoints: Vector[TransactionOutPoint]): DLCSign = {
      val outcomeSigs = sign.cetSignatures match {
        case CETSignaturesV0TLV(sigs) =>
          outcomes.zip(sigs)
      }

      val sigs = sign.fundingSignatures match {
        case FundingSignaturesV0TLV(witnesses) => witnesses
      }

      val fundingSigs = fundingOutPoints.zip(sigs)

      DLCSign(
        cetSigs = CETSignatures(
          outcomeSigs,
          PartialSignature(
            fundingPubKey,
            ECDigitalSignature(
              sign.refundSignature.bytes :+ HashType.sigHashAll.byte))),
        fundingSigs = FundingSignatures(fundingSigs),
        contractId = sign.contractId
      )
    }

    def fromTLV(sign: DLCSignTLV, offer: DLCOffer): DLCSign = {
      offer.contractInfo match {
        case info: SingleNonceContractInfo =>
          fromTLV(sign,
                  offer.pubKeys.fundingKey,
                  info.keys,
                  offer.fundingInputs.map(_.outPoint))
        case MultiNonceContractInfo(_, _, _, _) => ???
      }
    }

    def fromMessage(sign: LnMessage[DLCSignTLV], offer: DLCOffer): DLCSign = {
      fromTLV(sign.tlv, offer)
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
                val sig = ECAdaptorSignature(value.str)
                (EnumOutcome(key), sig)
              }

              val refundSig = getValue("refundSig")

              CETSignatures(
                outcomeSigs.toVector,
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
                  case (outPoint, scriptWitness) =>
                    (TransactionOutPoint(outPoint),
                     RawScriptWitnessParser
                       .read(scriptWitness.str)
                       .asInstanceOf[ScriptWitnessV0])
                }
              }
          }
          .get

      val contractId =
        vec
          .find(_._1 == "contractId")
          .map(obj => ByteVector.fromValidHex(obj._2.str))
          .get

      DLCSign(cetSigs, FundingSignatures(fundingSigs), contractId)
    }
  }

}
