package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.config.{NetworkParameters, Networks}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.SeqWrapper
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import scodec.bits.ByteVector

import scala.collection.immutable.HashMap

sealed trait DLCMessage

object DLCMessage {

  def calcParamHash(
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts): Sha256DigestBE = {
    CryptoUtil
      .sha256(oracleInfo.bytes ++ contractInfo.bytes ++ timeouts.bytes)
      .flip
  }

  sealed trait OracleInfo extends TLVSerializable[OracleInfoTLV] {
    def pubKey: SchnorrPublicKey

    /** The oracle's pre-committed nonces, in the correct order */
    def nonces: Vector[SchnorrNonce]

    /** The order of the given sigs should correspond to the given outcome. */
    def verifySigs(
        outcome: DLCOutcomeType,
        sigs: Vector[SchnorrDigitalSignature]): Boolean

    /** Computes the signature point (aka signature anticipation) for a given outcome.
      * This point is used for adaptor signing.
      */
    def sigPoint(outcome: DLCOutcomeType): ECPublicKey = {
      pubKey.computeSigPoint(outcome.serialized, nonces)
    }
  }

  case class SingleNonceOracleInfo(
      pubKey: SchnorrPublicKey,
      rValue: SchnorrNonce)
      extends OracleInfo
      with TLVSerializable[OracleInfoV0TLV] {
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
            pubKey.verify(CryptoUtil.sha256(outcome).bytes, sigs.head)
          }
        case UnsignedNumericOutcome(_) =>
          throw new IllegalArgumentException(
            s"Expected EnumOutcome, got $outcome")
      }
    }

    override def toTLV: OracleInfoV0TLV = OracleInfoV0TLV(pubKey, rValue)
  }

  object SingleNonceOracleInfo
      extends TLVDeserializable[OracleInfoV0TLV, SingleNonceOracleInfo](
        OracleInfoV0TLV) {

    override def fromTLV(tlv: OracleInfoV0TLV): SingleNonceOracleInfo = {
      SingleNonceOracleInfo(tlv.pubKey, tlv.rValue)
    }
  }

  case class MultiNonceOracleInfo(
      pubKey: SchnorrPublicKey,
      nonces: Vector[SchnorrNonce])
      extends OracleInfo
      with TLVSerializable[OracleInfoV1TLV] {
    require(nonces.nonEmpty, "Must contain positive number of nonces.")

    override def verifySigs(
        outcome: DLCOutcomeType,
        sigs: Vector[SchnorrDigitalSignature]): Boolean = {
      require(sigs.nonEmpty, "At least one signature is required")
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
                require(
                  sig.rx == nonce,
                  s"Unexpected nonce in ${sig.hex}, expected ${nonce.hex}")

                result && pubKey.verify(CryptoUtil.sha256(digit.toString).bytes,
                                        sig)
            }
      }
    }

    override def toTLV: OracleInfoV1TLV = OracleInfoV1TLV(pubKey, nonces)
  }

  object MultiNonceOracleInfo
      extends TLVDeserializable[OracleInfoV1TLV, MultiNonceOracleInfo](
        OracleInfoV1TLV) {

    override def fromTLV(tlv: OracleInfoV1TLV): MultiNonceOracleInfo = {
      MultiNonceOracleInfo(tlv.pubKey, tlv.nonces)
    }
  }

  object OracleInfo
      extends TLVDeserializable[OracleInfoTLV, OracleInfo](OracleInfoTLV) {

    val dummy: OracleInfo = SingleNonceOracleInfo(
      ECPublicKey.freshPublicKey.schnorrPublicKey,
      ECPublicKey.freshPublicKey.schnorrNonce)

    def fromOracleAnnouncement(
        announcement: OracleAnnouncementTLV): OracleInfo = {
      announcement.eventTLV.eventDescriptor match {
        case _: EnumEventDescriptorV0TLV | _: RangeEventDescriptorV0TLV =>
          require(announcement.eventTLV.nonces.size == 1)
          SingleNonceOracleInfo(announcement.publicKey,
                                announcement.eventTLV.nonces.head)
        case _: DigitDecompositionEventDescriptorV0TLV =>
          MultiNonceOracleInfo(announcement.publicKey,
                               announcement.eventTLV.nonces)
      }
    }

    override def fromTLV(tlv: OracleInfoTLV): OracleInfo = {
      tlv match {
        case tlv: OracleInfoV0TLV => SingleNonceOracleInfo.fromTLV(tlv)
        case tlv: OracleInfoV1TLV => MultiNonceOracleInfo.fromTLV(tlv)
      }
    }
  }

  sealed trait ContractInfo extends TLVSerializable[ContractInfoTLV] {

    /** Returns the counter-party's ContractInfo corresponding to this one.
      *
      * WARNING: this(outcome) + flip(TC)(outcome) is not guaranteed to equal TC.
      * As such, this should not be used to generate pairs of ContractInfos and
      * should only be used to replace a ContractInfo with another one of the flipped
      * perspective.
      * An example case is for MultiNonceContractInfo where flipping the interpolation points
      * could lead to an off-by-one after rounding so that the sum above gives TC-1.
      * In this example, only the offerer's ContractInfo should be used.
      */
    def flip(totalCollateral: Satoshis): ContractInfo
    def allOutcomes: Vector[DLCOutcomeType]
    def apply(outcome: DLCOutcomeType): Satoshis

    /** Returns the maximum payout this party could win from this contract */
    def max: Satoshis
  }

  case class SingleNonceContractInfo(
      outcomeValueMap: Vector[(EnumOutcome, Satoshis)])
      extends ContractInfo
      with TLVSerializable[ContractInfoV0TLV]
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

  object SingleNonceContractInfo
      extends TLVDeserializable[ContractInfoV0TLV, SingleNonceContractInfo](
        ContractInfoV0TLV) {

    def fromStringVec(
        vec: Vector[(String, Satoshis)]): SingleNonceContractInfo = {
      SingleNonceContractInfo(vec.map {
        case (str, amt) => EnumOutcome(str) -> amt
      })
    }

    override def fromTLV(tlv: ContractInfoV0TLV): SingleNonceContractInfo = {
      fromStringVec(tlv.outcomes)
    }
  }

  /** Contains a deterministically compressed set of outcomes computed from
    * a given payout curve.
    */
  case class MultiNonceContractInfo(
      outcomeValueFunc: DLCPayoutCurve,
      base: Int,
      numDigits: Int,
      totalCollateral: Satoshis)
      extends ContractInfo
      with TLVSerializable[ContractInfoV1TLV] {

    /** Vector is always the most significant digits */
    lazy val outcomeVec: Vector[(Vector[Int], Satoshis)] =
      CETCalculator.computeCETs(base,
                                numDigits,
                                outcomeValueFunc,
                                totalCollateral,
                                RoundingIntervals.noRounding)

    override def apply(outcome: DLCOutcomeType): Satoshis = {
      outcome match {
        case UnsignedNumericOutcome(digits) =>
          CETCalculator.searchForPrefix(digits, outcomeVec)(_._1) match {
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

    override lazy val allOutcomes: Vector[DLCOutcomeType] =
      outcomeVec.map { case (outcome, _) => UnsignedNumericOutcome(outcome) }

    override val max: Satoshis = totalCollateral

    override def flip(totalCollateral: Satoshis): MultiNonceContractInfo = {
      require(
        totalCollateral == this.totalCollateral,
        s"Input total collateral ($totalCollateral) did not match ${this.totalCollateral}")

      val flippedFunc = DLCPayoutCurve(outcomeValueFunc.points.map { point =>
        point.copy(payout = totalCollateral.toLong - point.payout)
      })

      MultiNonceContractInfo(
        flippedFunc,
        base,
        numDigits,
        totalCollateral
      )
    }

    override lazy val toTLV: ContractInfoV1TLV = {
      val tlvPoints = outcomeValueFunc.points.map { point =>
        TLVPoint(point.outcome,
                 point.roundedPayout,
                 point.extraPrecision,
                 point.isEndpoint)
      }

      ContractInfoV1TLV(base, numDigits, totalCollateral, tlvPoints)
    }
  }

  object MultiNonceContractInfo
      extends TLVDeserializable[ContractInfoV1TLV, MultiNonceContractInfo](
        ContractInfoV1TLV) {

    override def fromTLV(tlv: ContractInfoV1TLV): MultiNonceContractInfo = {
      val points = tlv.points.map { point =>
        val payoutWithPrecision =
          point.value.toLong + (BigDecimal(point.extraPrecision) / (1 << 16))
        OutcomePayoutPoint(point.outcome, payoutWithPrecision, point.isEndpoint)
      }

      MultiNonceContractInfo(DLCPayoutCurve(points),
                             tlv.base,
                             tlv.numDigits,
                             tlv.totalCollateral)
    }
  }

  object ContractInfo
      extends TLVDeserializable[ContractInfoTLV, ContractInfo](
        ContractInfoTLV) {

    val empty: ContractInfo = SingleNonceContractInfo(
      Vector(EnumOutcome("") -> Satoshis.zero))

    override def fromTLV(tlv: ContractInfoTLV): ContractInfo = {
      tlv match {
        case tlv: ContractInfoV0TLV => SingleNonceContractInfo.fromTLV(tlv)
        case tlv: ContractInfoV1TLV => MultiNonceContractInfo.fromTLV(tlv)
      }
    }
  }

  case class OracleAndContractInfo(
      oracleInfo: OracleInfo,
      offerContractInfo: ContractInfo,
      totalCollateral: Satoshis) {
    (oracleInfo, offerContractInfo) match {
      case (_: SingleNonceOracleInfo, info: SingleNonceContractInfo) =>
        require(
          info.max <= totalCollateral,
          s"Cannot have payout larger than totalCollateral ($totalCollateral): $info")
      case (_: MultiNonceOracleInfo, info: MultiNonceContractInfo) =>
        require(
          info.totalCollateral == totalCollateral,
          s"Expected total collateral of $totalCollateral, got ${info.totalCollateral}")
      case (_: OracleInfo, _: ContractInfo) =>
        throw new IllegalArgumentException(
          s"All infos must be for the same kind of outcome: $this")
    }

    def verifySigs(
        outcome: DLCOutcomeType,
        sigs: Vector[SchnorrDigitalSignature]): Boolean = {
      oracleInfo.verifySigs(outcome, sigs)
    }

    def findOutcome(
        sigs: Vector[SchnorrDigitalSignature]): Option[DLCOutcomeType] = {
      offerContractInfo match {
        case SingleNonceContractInfo(_) =>
          allOutcomes.find(verifySigs(_, sigs))
        case MultiNonceContractInfo(_, base, _, _) =>
          val digitsSigned = sigs.map { sig =>
            (0 until base)
              .find { possibleDigit =>
                oracleInfo.pubKey.verify(
                  CryptoUtil.sha256(possibleDigit.toString).bytes,
                  sig)
              }
              .getOrElse(throw new IllegalArgumentException(
                s"Signature $sig does not match any digit 0-${base - 1}"))
          }

          CETCalculator.searchForNumericOutcome(digitsSigned, allOutcomes)
      }
    }

    lazy val outcomeMap: Map[
      DLCOutcomeType,
      (ECPublicKey, Satoshis, Satoshis)] = {
      val builder =
        HashMap.newBuilder[DLCOutcomeType, (ECPublicKey, Satoshis, Satoshis)]

      allOutcomes.foreach { msg =>
        val offerPayout = offerContractInfo(msg)
        val acceptPayout = (totalCollateral - offerPayout).satoshis

        builder.+=((msg, (oracleInfo.sigPoint(msg), offerPayout, acceptPayout)))
      }

      builder.result()
    }

    def resultOfOutcome(
        outcome: DLCOutcomeType): (ECPublicKey, Satoshis, Satoshis) = {
      outcomeMap(outcome)
    }

    def sigPointForOutcome(outcome: DLCOutcomeType): ECPublicKey = {
      resultOfOutcome(outcome)._1
    }

    lazy val allOutcomes: Vector[DLCOutcomeType] =
      offerContractInfo.allOutcomes

    /** Returns the payouts for the signature as (toOffer, toAccept) */
    def getPayouts(
        sigs: Vector[SchnorrDigitalSignature]): (Satoshis, Satoshis) = {
      val outcome = findOutcome(sigs) match {
        case Some(outcome) => outcome
        case None =>
          throw new IllegalArgumentException(
            s"Signatures do not correspond to a possible outcome! $sigs")
      }
      getPayouts(outcome)
    }

    /** Returns the payouts for the outcome as (toOffer, toAccept) */
    def getPayouts(outcome: DLCOutcomeType): (Satoshis, Satoshis) = {
      val (_, offerOutcome, acceptOutcome) = resultOfOutcome(outcome)

      (offerOutcome, acceptOutcome)
    }

    def updateTotalCollateral(
        newTotalCollateral: Satoshis): OracleAndContractInfo = {
      if (newTotalCollateral == totalCollateral) {
        this
      } else {
        offerContractInfo match {
          case _: SingleNonceContractInfo =>
            this.copy(totalCollateral = newTotalCollateral)
          case info: MultiNonceContractInfo =>
            this.copy(offerContractInfo =
                        info.copy(totalCollateral = newTotalCollateral),
                      totalCollateral = newTotalCollateral)
        }
      }
    }
  }

  object OracleAndContractInfo {

    def apply(
        oracleInfo: OracleInfo,
        offerContractInfo: ContractInfo): OracleAndContractInfo = {
      OracleAndContractInfo(oracleInfo,
                            offerContractInfo,
                            offerContractInfo.max)
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

    def fromTLV(
        accept: DLCAcceptTLV,
        network: NetworkParameters,
        contractInfo: ContractInfo): DLCAccept = {
      contractInfo match {
        case info: SingleNonceContractInfo =>
          fromTLV(accept, network, info.keys)
        case multi: MultiNonceContractInfo =>
          fromTLV(accept, network, multi.allOutcomes)
      }
    }

    def fromTLV(accept: DLCAcceptTLV, offer: DLCOffer): DLCAccept = {
      fromTLV(accept, offer.changeAddress.networkParameters, offer.contractInfo)
    }

    def fromMessage(
        accept: LnMessage[DLCAcceptTLV],
        offer: DLCOffer): DLCAccept = {
      fromTLV(accept.tlv, offer)
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
        case multi: MultiNonceContractInfo =>
          fromTLV(sign,
                  offer.pubKeys.fundingKey,
                  multi.allOutcomes,
                  offer.fundingInputs.map(_.outPoint))
      }
    }

    def fromMessage(sign: LnMessage[DLCSignTLV], offer: DLCOffer): DLCSign = {
      fromTLV(sign.tlv, offer)
    }
  }

}
