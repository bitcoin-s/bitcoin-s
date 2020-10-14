package org.bitcoins.dlc.wallet.models

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  DLCAccept,
  DLCAcceptWithoutSigs
}
import org.bitcoins.commons.jsonmodels.dlc.{
  CETSignatures,
  DLCFundingInput,
  DLCPublicKeys
}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.tlv.DLCOutcomeType
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.{
  ECAdaptorSignature,
  ECPublicKey,
  Sha256Digest,
  Sha256DigestBE
}

case class DLCAcceptDb(
    paramHash: Sha256DigestBE,
    tempContractId: Sha256Digest,
    fundingKey: ECPublicKey,
    finalAddress: BitcoinAddress,
    totalCollateral: CurrencyUnit,
    changeAddress: BitcoinAddress) {

  def toDLCAccept(
      fundingInputs: Vector[DLCFundingInput],
      outcomeSigs: Vector[(DLCOutcomeType, ECAdaptorSignature)],
      refundSig: PartialSignature): DLCAccept = {
    val pubKeys =
      DLCPublicKeys(fundingKey, finalAddress)
    val cetSigs = CETSignatures(outcomeSigs, refundSig)
    DLCAccept(totalCollateral.satoshis,
              pubKeys,
              fundingInputs,
              changeAddress,
              cetSigs,
              tempContractId)
  }

  def toDLCAcceptWithoutSigs(
      tempContractId: Sha256Digest,
      fundingInputs: Vector[DLCFundingInput]): DLCAcceptWithoutSigs = {
    val pubKeys =
      DLCPublicKeys(fundingKey, finalAddress)

    DLCAcceptWithoutSigs(totalCollateral.satoshis,
                         pubKeys,
                         fundingInputs,
                         changeAddress,
                         tempContractId)
  }
}

object DLCAcceptDbHelper {

  def fromDLCAccept(
      paramHash: Sha256DigestBE,
      accept: DLCAccept): DLCAcceptDb = {
    DLCAcceptDb(
      paramHash,
      accept.tempContractId,
      accept.pubKeys.fundingKey,
      accept.pubKeys.payoutAddress,
      accept.totalCollateral,
      accept.changeAddress
    )
  }
}
