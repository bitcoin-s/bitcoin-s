package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.models.DLCMessage.DLCAccept.NegotiationFields
import org.bitcoins.core.protocol.dlc.models.DLCMessage._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv.NegotiationFieldsTLV
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto._

case class DLCAcceptDb(
    dlcId: Sha256Digest,
    fundingKey: ECPublicKey,
    payoutAddress: BitcoinAddress,
    payoutSerialId: UInt64,
    collateral: CurrencyUnit,
    changeAddress: BitcoinAddress,
    changeSerialId: UInt64,
    negotiationFieldsTLV: NegotiationFieldsTLV) {

  lazy val negotiationFields: NegotiationFields =
    NegotiationFields.fromTLV(negotiationFieldsTLV)

  def toDLCAccept(
      tempContractId: Sha256Digest,
      fundingInputs: Vector[DLCFundingInput],
      outcomeSigs: Vector[(ECPublicKey, ECAdaptorSignature)],
      refundSig: PartialSignature): DLCAccept = {
    val pubKeys =
      DLCPublicKeys(fundingKey, payoutAddress)
    val cetSigs = CETSignatures(outcomeSigs)
    DLCAccept(
      totalCollateral = collateral.satoshis,
      pubKeys = pubKeys,
      fundingInputs = fundingInputs,
      changeAddress = changeAddress,
      payoutSerialId = payoutSerialId,
      changeSerialId = changeSerialId,
      cetSigs = cetSigs,
      refundSig = refundSig,
      negotiationFields = negotiationFields,
      tempContractId = tempContractId
    )
  }

  def toDLCAcceptWithoutSigs(
      tempContractId: Sha256Digest,
      fundingInputs: Vector[DLCFundingInput]): DLCAcceptWithoutSigs = {
    val pubKeys =
      DLCPublicKeys(fundingKey, payoutAddress)

    DLCAcceptWithoutSigs(
      totalCollateral = collateral.satoshis,
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

object DLCAcceptDbHelper {

  def fromDLCAccept(id: Sha256Digest, accept: DLCAccept): DLCAcceptDb = {
    DLCAcceptDb(
      id,
      accept.pubKeys.fundingKey,
      accept.pubKeys.payoutAddress,
      accept.payoutSerialId,
      accept.totalCollateral,
      accept.changeAddress,
      accept.changeSerialId,
      accept.negotiationFields.toTLV
    )
  }
}
