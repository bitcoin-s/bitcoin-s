package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.config.{NetworkParameters, Networks}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.util.Random

sealed trait DLCMessage

object DLCMessage {

  def calcParamHash(
      contractInfo: ContractInfo,
      timeouts: DLCTimeouts): Sha256DigestBE = {
    CryptoUtil
      .sha256(contractInfo.bytes ++ timeouts.bytes)
      .flip
  }

  @tailrec
  def genSerialId(notEqualTo: Vector[UInt64] = Vector.empty): UInt64 = {
    val rand = {
      // Copy of Random.nextBytes(Int)
      // Not available for older versions
      val bytes = new Array[Byte](0 max 8)
      Random.nextBytes(bytes)
      bytes
    }
    val res = UInt64(ByteVector(rand))

    if (notEqualTo.contains(res)) genSerialId(notEqualTo)
    else res
  }

  @tailrec
  def genSerialIds(
      size: Int,
      notEqualTo: Vector[UInt64] = Vector.empty): Vector[UInt64] = {
    val ids = 0.until(size).toVector.map(_ => genSerialId(notEqualTo))

    if (ids.distinct.size != size)
      genSerialIds(size, notEqualTo)
    else ids
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

  /** The initiating party starts the protocol by sending an offer message to the other party.
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
      payoutSerialId: UInt64,
      changeSerialId: UInt64,
      fundOutputSerialId: UInt64,
      feeRate: SatoshisPerVirtualByte,
      timeouts: DLCTimeouts)
      extends DLCSetupMessage {

    require(
      fundingInputs.map(_.inputSerialId).distinct.size == fundingInputs.size,
      "All funding input serial ids must be unique")

    require(
      changeSerialId != fundOutputSerialId,
      s"changeSerialId ($changeSerialId) cannot be equal to fundOutputSerialId ($fundOutputSerialId)")

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
        payoutSerialId = payoutSerialId,
        totalCollateralSatoshis = totalCollateral,
        fundingInputs = fundingInputs.map(_.toTLV),
        changeSPK = changeAddress.scriptPubKey,
        changeSerialId = changeSerialId,
        fundOutputSerialId = fundOutputSerialId,
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
        payoutSerialId = offer.payoutSerialId,
        changeSerialId = offer.changeSerialId,
        fundOutputSerialId = offer.fundOutputSerialId,
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
      payoutSerialId: UInt64,
      changeSerialId: UInt64,
      negotiationFields: DLCAccept.NegotiationFields,
      tempContractId: Sha256Digest) {

    def withSigs(cetSigs: CETSignatures): DLCAccept = {
      DLCAccept(
        totalCollateral = totalCollateral,
        pubKeys = pubKeys,
        fundingInputs = fundingInputs,
        changeAddress = changeAddress,
        payoutSerialId = payoutSerialId,
        changeSerialId = changeSerialId,
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
      payoutSerialId: UInt64,
      changeSerialId: UInt64,
      cetSigs: CETSignatures,
      negotiationFields: DLCAccept.NegotiationFields,
      tempContractId: Sha256Digest)
      extends DLCSetupMessage {

    require(
      fundingInputs.map(_.inputSerialId).distinct.size == fundingInputs.size,
      "All funding input serial ids must be unique")

    def toTLV: DLCAcceptTLV = {
      DLCAcceptTLV(
        tempContractId = tempContractId,
        totalCollateralSatoshis = totalCollateral,
        fundingPubKey = pubKeys.fundingKey,
        payoutSPK = pubKeys.payoutAddress.scriptPubKey,
        payoutSerialId = payoutSerialId,
        fundingInputs = fundingInputs.map(_.toTLV),
        changeSPK = changeAddress.scriptPubKey,
        changeSerialId = changeSerialId,
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
      DLCAcceptWithoutSigs(
        totalCollateral = totalCollateral,
        pubKeys = pubKeys,
        fundingInputs = fundingInputs,
        changeAddress = changeAddress,
        payoutSerialId = payoutSerialId,
        changeSerialId = changeSerialId,
        negotiationFields = negotiationFields,
        tempContractId = tempContractId
      )
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
        adaptorPoints: Vector[ECPublicKey]): DLCAccept = {
      val outcomeSigs = accept.cetSignatures match {
        case CETSignaturesV0TLV(sigs) =>
          adaptorPoints.zip(sigs)
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
        payoutSerialId = accept.payoutSerialId,
        changeSerialId = accept.changeSerialId,
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
      fromTLV(accept, network, contractInfo.adaptorPoints)
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
        adaptorPoints: Vector[ECPublicKey],
        fundingOutPoints: Vector[TransactionOutPoint]): DLCSign = {
      val outcomeSigs = sign.cetSignatures match {
        case CETSignaturesV0TLV(sigs) =>
          adaptorPoints.zip(sigs)
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
              offer.contractInfo.adaptorPoints,
              offer.fundingInputs.map(_.outPoint))
    }

    def fromMessage(sign: LnMessage[DLCSignTLV], offer: DLCOffer): DLCSign = {
      fromTLV(sign.tlv, offer)
    }
  }

}
