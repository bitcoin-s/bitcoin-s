package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.models.DLCMessage.{
  DLCAccept,
  DLCAcceptWithoutSigs
}
import org.bitcoins.core.protocol.dlc.models.{
  CETSignatures,
  DLCFundingInput,
  DLCPublicKeys
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto._

case class DLCAcceptDb(
    paramHash: Sha256DigestBE,
    tempContractId: Sha256Digest,
    fundingKey: ECPublicKey,
    finalAddress: BitcoinAddress,
    payoutSerialId: UInt64,
    totalCollateral: CurrencyUnit,
    changeAddress: BitcoinAddress,
    changeSerialId: UInt64) {

  private[wallet] def toDLCAccept(
      fundingInputs: Vector[DLCFundingInput],
      outcomeSigs: Vector[(ECPublicKey, ECAdaptorSignature)],
      refundSig: PartialSignature): DLCAccept = {
    val pubKeys =
      DLCPublicKeys(fundingKey, finalAddress)
    val cetSigs = CETSignatures(outcomeSigs, refundSig)
    DLCAccept(
      totalCollateral = totalCollateral.satoshis,
      pubKeys = pubKeys,
      fundingInputs = fundingInputs,
      changeAddress = changeAddress,
      payoutSerialId = payoutSerialId,
      changeSerialId = changeSerialId,
      cetSigs = cetSigs,
      negotiationFields = DLCAccept.NoNegotiationFields,
      tempContractId = tempContractId
    )
  }

  def toDLCAcceptWithoutSigs(
      tempContractId: Sha256Digest,
      fundingInputs: Vector[DLCFundingInput]): DLCAcceptWithoutSigs = {
    val pubKeys =
      DLCPublicKeys(fundingKey, finalAddress)

    DLCAcceptWithoutSigs(
      totalCollateral = totalCollateral.satoshis,
      pubKeys = pubKeys,
      fundingInputs = fundingInputs,
      changeAddress = changeAddress,
      payoutSerialId = payoutSerialId,
      changeSerialId = changeSerialId,
      negotiationFields = DLCAccept.NoNegotiationFields,
      tempContractId = tempContractId
    )
  }
}

object DLCAcceptDbHelper {

  def fromDLCAccept(
      paramHash: Sha256DigestBE,
      accept: DLCAccept): DLCAcceptDb = {
    DLCAcceptDb(
      paramHash = paramHash,
      tempContractId = accept.tempContractId,
      fundingKey = accept.pubKeys.fundingKey,
      finalAddress = accept.pubKeys.payoutAddress,
      payoutSerialId = accept.payoutSerialId,
      totalCollateral = accept.totalCollateral,
      changeAddress = accept.changeAddress,
      changeSerialId = accept.changeSerialId
    )
  }
}
