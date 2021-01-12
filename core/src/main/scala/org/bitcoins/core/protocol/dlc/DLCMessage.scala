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
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts): Sha256DigestBE = {
    CryptoUtil
      .sha256(contractInfo.bytes ++ timeouts.bytes)
      .flip
  }

  sealed trait OracleInfo extends TLVSerializable[OracleInfoTLV]

  sealed trait EnumOracleInfo extends OracleInfo
  sealed trait NumericOracleInfo extends OracleInfo

  object OracleInfo
      extends TLVDeserializable[OracleInfoTLV, OracleInfo](OracleInfoTLV) {

    override def fromTLV(tlv: OracleInfoTLV): OracleInfo = {
      tlv match {
        case tlv: OracleInfoV0TLV => SingleOracleInfo.fromTLV(tlv)
        case tlv: OracleInfoV1TLV => ExactMultiOracleInfo.fromTLV(tlv)
        case tlv: OracleInfoV2TLV => NumericMultiOracleInfo.fromTLV(tlv)
      }
    }
  }

  sealed trait SingleOracleInfo
      extends OracleInfo
      with TLVSerializable[OracleInfoV0TLV] {
    def announcement: OracleAnnouncementTLV
    def publicKey: SchnorrPublicKey = announcement.publicKey

    /** The oracle's pre-committed nonces, in the correct order */
    def nonces: Vector[SchnorrNonce] = announcement.eventTLV.nonces

    /** The order of the given sigs should correspond to the given outcome. */
    def verifySigs(outcome: DLCOutcomeType, sigs: OracleSignatures): Boolean

    /** Computes the signature point (aka signature anticipation) for a given outcome.
      * This point is used for adaptor signing.
      */
    def sigPoint(outcome: DLCOutcomeType): ECPublicKey = {
      publicKey.computeSigPoint(outcome.serialized, nonces)
    }

    override def toTLV: OracleInfoV0TLV = OracleInfoV0TLV(announcement)
  }

  object SingleOracleInfo
      extends TLVDeserializable[OracleInfoV0TLV, SingleOracleInfo](
        OracleInfoV0TLV) {

    def apply(announcement: OracleAnnouncementTLV): SingleOracleInfo = {
      announcement.eventTLV.eventDescriptor match {
        case _: EnumEventDescriptorV0TLV =>
          EnumSingleOracleInfo(announcement)
        case _: NumericEventDescriptorTLV =>
          NumericSingleOracleInfo(announcement)
      }
    }

    def apply(tlv: OracleInfoV0TLV): SingleOracleInfo = {
      SingleOracleInfo(tlv.announcement)
    }

    override def fromTLV(tlv: OracleInfoV0TLV): SingleOracleInfo = {
      SingleOracleInfo(tlv)
    }
  }

  case class EnumSingleOracleInfo(announcement: OracleAnnouncementTLV)
      extends SingleOracleInfo
      with EnumOracleInfo {
    require(announcement.eventTLV.eventDescriptor
              .isInstanceOf[EnumEventDescriptorV0TLV],
            s"Enum OracleInfo requires EnumEventDescriptor, $announcement")

    val nonce: SchnorrNonce = announcement.eventTLV.nonces.head

    override def verifySigs(
        outcome: DLCOutcomeType,
        sigs: OracleSignatures): Boolean = {
      outcome match {
        case EnumOutcome(outcome) =>
          sigs match {
            case _: NumericOracleSignatures =>
              throw new IllegalArgumentException(
                s"Expected one signature, got $sigs")
            case EnumOracleSignature(_, sig) =>
              if (sig.rx != nonce) {
                throw new IllegalArgumentException(
                  s"Expected R value of $nonce, got $sig")
              } else {
                publicKey.verify(CryptoUtil.sha256DLCAttestation(outcome).bytes,
                                 sig)
              }
          }
        case UnsignedNumericOutcome(_) =>
          throw new IllegalArgumentException(
            s"Expected EnumOutcome, got $outcome")
      }
    }
  }

  object EnumSingleOracleInfo
      extends TLVDeserializable[OracleInfoV0TLV, EnumSingleOracleInfo](
        OracleInfoV0TLV) {

    def dummyForKeys(
        privKey: ECPrivateKey,
        nonce: SchnorrNonce,
        events: Vector[EnumOutcome]): EnumSingleOracleInfo = {
      EnumSingleOracleInfo(
        OracleAnnouncementV0TLV
          .dummyForEventsAndKeys(privKey, nonce, events))
    }

    override def fromTLV(tlv: OracleInfoV0TLV): EnumSingleOracleInfo = {
      EnumSingleOracleInfo(tlv.announcement)
    }
  }

  case class NumericSingleOracleInfo(announcement: OracleAnnouncementTLV)
      extends SingleOracleInfo
      with NumericOracleInfo {
    require(
      announcement.eventTLV.eventDescriptor
        .isInstanceOf[NumericEventDescriptorTLV],
      s"Numeric OracleInfo requires NumericEventDescriptor, $announcement")

    override def verifySigs(
        outcome: DLCOutcomeType,
        sigs: OracleSignatures): Boolean = {
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

                result && publicKey.verify(
                  CryptoUtil.sha256DLCAttestation(digit.toString).bytes,
                  sig)
            }
      }
    }
  }

  object NumericSingleOracleInfo {

    def dummyForKeys(
        privKey: ECPrivateKey,
        nonces: Vector[SchnorrNonce]): NumericSingleOracleInfo = {
      NumericSingleOracleInfo(
        OracleAnnouncementV0TLV.dummyForKeys(privKey, nonces))
    }
  }

  sealed trait MultiOracleInfo[+T <: SingleOracleInfo]
      extends OracleInfo
      with TLVSerializable[MultiOracleInfoTLV] {
    def announcements: Vector[OracleAnnouncementTLV]

    // Override this with a val to invoke requirements
    def singleOracleInfos: Vector[T]
  }

  sealed trait ExactMultiOracleInfo[+T <: SingleOracleInfo]
      extends MultiOracleInfo[T]
      with TLVSerializable[OracleInfoV1TLV] {
    override def toTLV: OracleInfoV1TLV = OracleInfoV1TLV(announcements)
  }

  object ExactMultiOracleInfo
      extends TLVDeserializable[
        OracleInfoV1TLV,
        ExactMultiOracleInfo[SingleOracleInfo]](OracleInfoV1TLV) {

    def apply(tlv: OracleInfoV1TLV): ExactMultiOracleInfo[SingleOracleInfo] = {
      tlv.oracles.head.eventTLV.eventDescriptor match {
        case _: EnumEventDescriptorV0TLV => EnumMultiOracleInfo(tlv.oracles)
        case _: NumericEventDescriptorTLV =>
          NumericExactMultiOracleInfo(tlv.oracles)
      }
    }

    override def fromTLV(
        tlv: OracleInfoV1TLV): ExactMultiOracleInfo[SingleOracleInfo] = {
      ExactMultiOracleInfo(tlv)
    }
  }

  case class EnumMultiOracleInfo(announcements: Vector[OracleAnnouncementTLV])
      extends ExactMultiOracleInfo[EnumSingleOracleInfo]
      with EnumOracleInfo {

    override val singleOracleInfos: Vector[EnumSingleOracleInfo] =
      announcements.map(EnumSingleOracleInfo.apply)
  }

  case class NumericExactMultiOracleInfo(
      announcements: Vector[OracleAnnouncementTLV])
      extends ExactMultiOracleInfo[NumericSingleOracleInfo]
      with NumericOracleInfo {

    val singleOracleInfos: Vector[NumericSingleOracleInfo] =
      announcements.map(NumericSingleOracleInfo.apply)
  }

  case class NumericMultiOracleInfo(
      announcements: Vector[OracleAnnouncementTLV],
      maxErrorExp: Int,
      minFailExp: Int,
      maximizeCoverage: Boolean)
      extends MultiOracleInfo[NumericSingleOracleInfo]
      with TLVSerializable[OracleInfoV2TLV]
      with NumericOracleInfo {

    override val singleOracleInfos: Vector[NumericSingleOracleInfo] =
      announcements.map(NumericSingleOracleInfo.apply)

    override def toTLV: OracleInfoV2TLV = {
      OracleInfoV2TLV(
        announcements,
        OracleParamsV0TLV(maxErrorExp, minFailExp, maximizeCoverage))
    }
  }

  object NumericMultiOracleInfo
      extends TLVDeserializable[OracleInfoV2TLV, NumericMultiOracleInfo](
        OracleInfoV2TLV) {

    def apply(
        announcements: Vector[OracleAnnouncementTLV],
        params: OracleParamsTLV): NumericMultiOracleInfo = {
      params match {
        case OracleParamsV0TLV(maxErrorExp, minFailExp, maximizeCoverage) =>
          NumericMultiOracleInfo(announcements,
                                 maxErrorExp,
                                 minFailExp,
                                 maximizeCoverage)
      }
    }

    override def fromTLV(tlv: OracleInfoV2TLV): NumericMultiOracleInfo = {
      NumericMultiOracleInfo(tlv.oracles, tlv.params)
    }
  }

  sealed trait ContractDescriptor
      extends TLVSerializable[ContractDescriptorTLV] {

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
    def flip(totalCollateral: Satoshis): ContractDescriptor
  }

  object ContractDescriptor
      extends TLVDeserializable[ContractDescriptorTLV, ContractDescriptor](
        ContractDescriptorTLV) {

    val empty: ContractDescriptor = EnumContractDescriptor(
      Vector(EnumOutcome("") -> Satoshis.zero))

    override def fromTLV(tlv: ContractDescriptorTLV): ContractDescriptor = {
      tlv match {
        case tlv: ContractDescriptorV0TLV => EnumContractDescriptor.fromTLV(tlv)
        case tlv: ContractDescriptorV1TLV =>
          NumericContractDescriptor.fromTLV(tlv)
      }
    }
  }

  case class EnumContractDescriptor(
      outcomeValueMap: Vector[(EnumOutcome, Satoshis)])
      extends ContractDescriptor
      with TLVSerializable[ContractDescriptorV0TLV]
      with SeqWrapper[(EnumOutcome, Satoshis)] {

    override def wrapped: Vector[(EnumOutcome, Satoshis)] = outcomeValueMap

    def keys: Vector[EnumOutcome] = outcomeValueMap.map(_._1)

    def values: Vector[Satoshis] = outcomeValueMap.map(_._2)

    override def toTLV: ContractDescriptorV0TLV =
      ContractDescriptorV0TLV(outcomeValueMap.map {
        case (outcome, amt) => outcome.outcome -> amt
      })

    override def flip(totalCollateral: Satoshis): EnumContractDescriptor = {
      EnumContractDescriptor(outcomeValueMap.map {
        case (hash, amt) => (hash, (totalCollateral - amt).satoshis)
      })
    }
  }

  object EnumContractDescriptor
      extends TLVDeserializable[
        ContractDescriptorV0TLV,
        EnumContractDescriptor](ContractDescriptorV0TLV) {

    def fromStringVec(
        vec: Vector[(String, Satoshis)]): EnumContractDescriptor = {
      EnumContractDescriptor(vec.map {
        case (str, amt) => EnumOutcome(str) -> amt
      })
    }

    override def fromTLV(
        tlv: ContractDescriptorV0TLV): EnumContractDescriptor = {
      fromStringVec(tlv.outcomes)
    }
  }

  /** Contains a deterministically compressed set of outcomes computed from
    * a given payout curve.
    */
  case class NumericContractDescriptor(
      outcomeValueFunc: DLCPayoutCurve,
      numDigits: Int,
      roundingIntervals: RoundingIntervals)
      extends ContractDescriptor
      with TLVSerializable[ContractDescriptorV1TLV] {

    override def flip(totalCollateral: Satoshis): NumericContractDescriptor = {

      val flippedFunc = DLCPayoutCurve(outcomeValueFunc.points.map { point =>
        point.copy(payout = totalCollateral.toLong - point.payout)
      })

      NumericContractDescriptor(
        flippedFunc,
        numDigits,
        roundingIntervals
      )
    }

    override def toTLV: ContractDescriptorV1TLV = {
      ContractDescriptorV1TLV(numDigits,
                              outcomeValueFunc.toTLV,
                              roundingIntervals.toTLV)
    }
  }

  object NumericContractDescriptor
      extends TLVDeserializable[
        ContractDescriptorV1TLV,
        NumericContractDescriptor](ContractDescriptorV1TLV) {

    override def fromTLV(
        tlv: ContractDescriptorV1TLV): NumericContractDescriptor = {
      NumericContractDescriptor(
        DLCPayoutCurve.fromTLV(tlv.payoutFunction),
        tlv.numDigits,
        RoundingIntervals.fromTLV(tlv.roundingIntervals)
      )
    }
  }

  case class ContractInfo(
      totalCollateral: Satoshis,
      contractDescriptor: ContractDescriptor,
      oracleInfo: OracleInfo)
      extends TLVSerializable[ContractInfoV0TLV] {

    override def toTLV: ContractInfoV0TLV = {
      ContractInfoV0TLV(totalCollateral,
                        contractDescriptor.toTLV,
                        oracleInfo.toTLV)
    }

    /** Returns the maximum payout this party could win from this contract */
    val max: Satoshis = contractDescriptor match {
      case descriptor: EnumContractDescriptor =>
        descriptor.values.maxBy(_.toLong)
      case _: NumericContractDescriptor => totalCollateral
    }

    val descriptorAndInfo: Either[
      (EnumContractDescriptor, EnumOracleInfo),
      (NumericContractDescriptor, NumericOracleInfo)] =
      (contractDescriptor, oracleInfo) match {
        case (contractDescriptor: EnumContractDescriptor,
              oracleInfo: EnumOracleInfo) =>
          Left((contractDescriptor, oracleInfo))
        case (contractDescriptor: NumericContractDescriptor,
              oracleInfo: NumericOracleInfo) =>
          Right((contractDescriptor, oracleInfo))
        case (_: ContractDescriptor, _: OracleInfo) =>
          throw new IllegalArgumentException(
            s"All infos must be for the same kind of outcome: $this")
      }

    /** Vector is always the most significant digits */
    lazy val allOutcomesAndPayouts: Vector[(OracleOutcome, Satoshis)] = {
      descriptorAndInfo match {
        case Left(
              (descriptor: EnumContractDescriptor,
               oracleInfo: EnumSingleOracleInfo)) =>
          descriptor.keys.map { outcome =>
            // Safe .get because outcome is from descriptor.keys
            val payout = descriptor.find(_._1 == outcome).get._2
            (EnumOracleOutcome(Vector(oracleInfo), outcome), payout)
          }
        case Left(
              (descriptor: EnumContractDescriptor,
               oracleInfo: EnumMultiOracleInfo)) =>
          descriptor.keys.map { outcome =>
            // Safe .get because outcome is from descriptor.keys
            val payout = descriptor.find(_._1 == outcome).get._2
            (EnumOracleOutcome(oracleInfo.singleOracleInfos, outcome), payout)
          }
        case Right(
              (descriptor: NumericContractDescriptor,
               oracleInfo: NumericSingleOracleInfo)) =>
          val vec = CETCalculator.computeCETs(base = 2,
                                              descriptor.numDigits,
                                              descriptor.outcomeValueFunc,
                                              totalCollateral,
                                              descriptor.roundingIntervals)

          vec.map {
            case (digits, amt) =>
              (NumericOracleOutcome(oracleInfo, UnsignedNumericOutcome(digits)),
               amt)
          }
        case Right(
              (descriptor: NumericContractDescriptor,
               oracleInfo: NumericExactMultiOracleInfo)) =>
          val vec = CETCalculator.computeCETs(base = 2,
                                              descriptor.numDigits,
                                              descriptor.outcomeValueFunc,
                                              totalCollateral,
                                              descriptor.roundingIntervals)

          vec.map {
            case (digits, amt) =>
              val outcome = UnsignedNumericOutcome(digits)
              (NumericOracleOutcome(
                 oracleInfo.singleOracleInfos.map((_, outcome))),
               amt)
          }
        case Right(
              (descriptor: NumericContractDescriptor,
               oracleInfo: NumericMultiOracleInfo)) =>
          // FIXME
          throw new UnsupportedOperationException(s"$descriptor, $oracleInfo")
      }
    }

    lazy val allOutcomes: Vector[OracleOutcome] =
      allOutcomesAndPayouts.map(_._1)

    val sigPointMap: Map[ECPublicKey, OracleOutcome] =
      allOutcomes.map(outcome => outcome.sigPoint -> outcome).toMap

    lazy val outcomeMap: Map[
      OracleOutcome,
      (ECPublicKey, Satoshis, Satoshis)] = {
      val builder =
        HashMap.newBuilder[OracleOutcome, (ECPublicKey, Satoshis, Satoshis)]

      allOutcomesAndPayouts.foreach {
        case (outcome, offerPayout) =>
          val acceptPayout = (totalCollateral - offerPayout).satoshis
          val adaptorPoint = outcome.sigPoint

          builder.+=(outcome -> (adaptorPoint, offerPayout, acceptPayout))
      }

      builder.result()
    }

    def verifySigs(
        outcome: OracleOutcome,
        sigs: Vector[OracleSignatures]): Boolean = {
      outcome match {
        case EnumOracleOutcome(_, enumOutcome) =>
          oracleInfo match {
            case _: NumericOracleInfo =>
              throw new IllegalArgumentException(
                s"Cannot handle $enumOutcome with numeric oracle commitments: $oracleInfo")
            case _: EnumOracleInfo =>
              sigs.foldLeft(true) {
                case (boolSoFar, sig) =>
                  boolSoFar && sig.verifySignatures(enumOutcome)
              }
          }
        case NumericOracleOutcome(oraclesAndOutcomes) =>
          oracleInfo match {
            case _: EnumOracleInfo =>
              throw new IllegalArgumentException(
                s"Cannot handle numeric outcomes with enum oracle commitments: $oracleInfo")
            case _: NumericOracleInfo =>
              sigs.foldLeft(true) {
                case (boolSoFar, sig) =>
                  lazy val numericOutcome =
                    oraclesAndOutcomes.find(_._1 == sig.oracle).get._2
                  boolSoFar && sig.verifySignatures(numericOutcome)
              }
          }
      }
    }

    def findOutcome(sigs: Vector[OracleSignatures]): Option[OracleOutcome] = {
      // TODO: Optimize by looking at nonces
      // TODO: Optimize using NumericOracleSignatures.computeOutcome
      allOutcomes.find(verifySigs(_, sigs))
    }

    def resultOfOutcome(
        outcome: OracleOutcome): (ECPublicKey, Satoshis, Satoshis) = {
      outcomeMap(outcome)
    }

    /** Returns the payouts for the signature as (toOffer, toAccept) */
    def getPayouts(sigs: Vector[OracleSignatures]): (Satoshis, Satoshis) = {
      val outcome = findOutcome(sigs) match {
        case Some(outcome) => outcome
        case None =>
          throw new IllegalArgumentException(
            s"Signatures do not correspond to a possible outcome! $sigs")
      }
      getPayouts(outcome)
    }

    /** Returns the payouts for the outcome as (toOffer, toAccept) */
    def getPayouts(outcome: OracleOutcome): (Satoshis, Satoshis) = {
      val (_, offerOutcome, acceptOutcome) = resultOfOutcome(outcome)

      (offerOutcome, acceptOutcome)
    }

    def updateOnAccept(
        newTotalCollateral: Satoshis,
        negotiationFields: DLCAccept.NegotiationFields): ContractInfo = {
      if (newTotalCollateral == totalCollateral) {
        this
      } else {
        contractDescriptor match {
          case _: EnumContractDescriptor =>
            if (negotiationFields != DLCAccept.NoNegotiationFields) {
              throw new IllegalArgumentException(
                s"Cannot have rounding intervals for single nonce contract: $negotiationFields")
            }
            this.copy(totalCollateral = newTotalCollateral)
          case descriptor: NumericContractDescriptor =>
            val newRoundingIntervals = negotiationFields match {
              case DLCAccept.NegotiationFieldsV1(acceptRoundingIntervals) =>
                descriptor.roundingIntervals.minRoundingWith(
                  acceptRoundingIntervals)
              case DLCAccept.NoNegotiationFields => descriptor.roundingIntervals
            }
            this.copy(
              totalCollateral = newTotalCollateral,
              contractDescriptor =
                descriptor.copy(roundingIntervals = newRoundingIntervals))
        }
      }
    }
  }

  object ContractInfo
      extends TLVDeserializable[ContractInfoV0TLV, ContractInfo](
        ContractInfoV0TLV) {

    lazy val dummy: ContractInfo = fromTLV(ContractInfoV0TLV.dummy)

    override def fromTLV(tlv: ContractInfoV0TLV): ContractInfo = {
      ContractInfo(tlv.totalCollateral,
                   ContractDescriptor.fromTLV(tlv.contractDescriptor),
                   OracleInfo.fromTLV(tlv.oracleInfo))
    }

    def apply(
        enumDescriptor: EnumContractDescriptor,
        enumOracleInfo: EnumOracleInfo): ContractInfo = {
      ContractInfo(totalCollateral = enumDescriptor.values.maxBy(_.toLong),
                   enumDescriptor,
                   enumOracleInfo)
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
    * @param contractInfo The oracle public key and R point(s) to use to build the CETs as
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
      contractInfo: ContractInfo,
      pubKeys: DLCPublicKeys,
      totalCollateral: Satoshis,
      fundingInputs: Vector[DLCFundingInput],
      changeAddress: BitcoinAddress,
      feeRate: SatoshisPerVirtualByte,
      timeouts: DLCTimeouts)
      extends DLCSetupMessage {

    val oracleInfo: OracleInfo = contractInfo.oracleInfo
    val contractDescriptor: ContractDescriptor = contractInfo.contractDescriptor

    lazy val paramHash: Sha256DigestBE = calcParamHash(contractInfo, timeouts)

    val tempContractId: Sha256Digest =
      CryptoUtil.sha256(toMessage.bytes)

    def toTLV: DLCOfferTLV = {
      val chainHash =
        changeAddress.networkParameters.chainParams.genesisBlock.blockHeader.hash

      DLCOfferTLV(
        contractFlags = 0x00,
        chainHash = chainHash,
        contractInfo.toTLV,
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

      DLCOffer(
        contractInfo = contractInfo,
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
      negotiationFields: DLCAccept.NegotiationFields,
      tempContractId: Sha256Digest) {

    def withSigs(cetSigs: CETSignatures): DLCAccept = {
      DLCAccept(
        totalCollateral = totalCollateral,
        pubKeys = pubKeys,
        fundingInputs = fundingInputs,
        changeAddress = changeAddress,
        cetSigs = cetSigs,
        negotiationFields = negotiationFields,
        tempContractId = tempContractId
      )
    }
  }

  case class DLCAccept(
      totalCollateral: Satoshis,
      pubKeys: DLCPublicKeys,
      fundingInputs: Vector[DLCFundingInput],
      changeAddress: BitcoinAddress,
      cetSigs: CETSignatures,
      negotiationFields: DLCAccept.NegotiationFields,
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
        refundSignature = ECDigitalSignature.fromFrontOfBytes(
          cetSigs.refundSig.signature.bytes),
        negotiationFields = negotiationFields.toTLV
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
                           negotiationFields,
                           tempContractId)
    }
  }

  object DLCAccept {

    sealed trait NegotiationFields extends TLVSerializable[NegotiationFieldsTLV]

    case object NoNegotiationFields
        extends TLVSerializable[NoNegotiationFieldsTLV.type]
        with NegotiationFields {
      override def toTLV: NoNegotiationFieldsTLV.type = NoNegotiationFieldsTLV
    }

    case class NegotiationFieldsV1(roundingIntervals: RoundingIntervals)
        extends TLVSerializable[NegotiationFieldsV1TLV]
        with NegotiationFields {

      override def toTLV: NegotiationFieldsV1TLV =
        NegotiationFieldsV1TLV(roundingIntervals.toTLV)
    }

    object NegotiationFields {

      def fromTLV(tlv: NegotiationFieldsTLV): NegotiationFields = {
        tlv match {
          case NoNegotiationFieldsTLV => NoNegotiationFields
          case NegotiationFieldsV1TLV(roundingIntervalsTLV) =>
            NegotiationFieldsV1(RoundingIntervals.fromTLV(roundingIntervalsTLV))
        }
      }
    }

    def fromTLV(
        accept: DLCAcceptTLV,
        network: NetworkParameters,
        outcomes: Vector[OracleOutcome]): DLCAccept = {
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
        negotiationFields = NegotiationFields.fromTLV(accept.negotiationFields),
        tempContractId = accept.tempContractId
      )
    }

    def fromTLV(
        accept: DLCAcceptTLV,
        network: NetworkParameters,
        contractInfo: ContractInfo): DLCAccept = {
      fromTLV(accept, network, contractInfo.allOutcomes)
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
        outcomes: Vector[OracleOutcome],
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
      fromTLV(sign,
              offer.pubKeys.fundingKey,
              offer.contractInfo.allOutcomes,
              offer.fundingInputs.map(_.outPoint))
    }

    def fromMessage(sign: LnMessage[DLCSignTLV], offer: DLCOffer): DLCSign = {
      fromTLV(sign.tlv, offer)
    }
  }

}
