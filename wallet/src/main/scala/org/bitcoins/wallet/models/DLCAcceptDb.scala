package org.bitcoins.wallet.models

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  DLCAccept,
  DLCAcceptWithoutSigs
}
import org.bitcoins.commons.jsonmodels.dlc.{CETSignatures, DLCPublicKeys}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.OutputReference
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.{ECAdaptorSignature, ECPublicKey, Sha256DigestBE}

case class DLCAcceptDb(
    eventId: Sha256DigestBE,
    fundingKey: ECPublicKey,
    finalAddress: BitcoinAddress,
    totalCollateral: CurrencyUnit,
    changeAddress: BitcoinAddress) {

  def toDLCAccept(
      fundingInputs: Vector[OutputReference],
      outcomeSigs: Map[Sha256DigestBE, ECAdaptorSignature],
      refundSig: PartialSignature): DLCAccept = {
    val pubKeys =
      DLCPublicKeys(fundingKey, finalAddress)
    val cetSigs = CETSignatures(outcomeSigs, refundSig)
    DLCAccept(totalCollateral.satoshis,
              pubKeys,
              fundingInputs,
              changeAddress,
              cetSigs,
              eventId)
  }

  def toDLCAcceptWithoutSigs(
      fundingInputs: Vector[OutputReference]): DLCAcceptWithoutSigs = {
    val pubKeys =
      DLCPublicKeys(fundingKey, finalAddress)

    DLCAcceptWithoutSigs(totalCollateral.satoshis,
                         pubKeys,
                         fundingInputs,
                         changeAddress,
                         eventId)
  }
}

object DLCAcceptDbHelper {

  def fromDLCAccept(accept: DLCAccept): DLCAcceptDb = {
    DLCAcceptDb(
      accept.eventId,
      accept.pubKeys.fundingKey,
      accept.pubKeys.payoutAddress,
      accept.totalCollateral,
      accept.changeAddress
    )
  }
}
