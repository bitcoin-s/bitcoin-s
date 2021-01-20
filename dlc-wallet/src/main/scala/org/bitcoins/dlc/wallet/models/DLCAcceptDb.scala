package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.DLCMessage._
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto._

case class DLCAcceptDb(
    paramHash: Sha256DigestBE,
    tempContractId: Sha256Digest,
    fundingKey: ECPublicKey,
    finalAddress: BitcoinAddress,
    totalCollateral: CurrencyUnit,
    changeAddress: BitcoinAddress) {

  def toDLCAccept(
      fundingInputs: Vector[DLCFundingInput],
      outcomeSigs: Vector[(OracleOutcome, ECAdaptorSignature)],
      refundSig: PartialSignature): DLCAccept = {
    val pubKeys =
      DLCPublicKeys(fundingKey, finalAddress)
    val cetSigs = CETSignatures(outcomeSigs, refundSig)
    DLCAccept(totalCollateral.satoshis,
              pubKeys,
              fundingInputs,
              changeAddress,
              cetSigs,
              DLCAccept.NoNegotiationFields,
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
                         DLCAccept.NoNegotiationFields,
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
