package org.bitcoins.wallet.models

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.DLCAccept
import org.bitcoins.commons.jsonmodels.dlc.{CETSignatures, DLCPublicKeys}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.OutputReference
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.{ECPublicKey, Sha256DigestBE}

case class DLCAcceptDb(
    eventId: Sha256DigestBE,
    fundingKey: ECPublicKey,
    toLocalCETKey: ECPublicKey,
    finalAddress: BitcoinAddress,
    totalCollateral: CurrencyUnit,
    refundSig: PartialSignature,
    changeAddress: BitcoinAddress) {

  def toDLCAccept(
      fundingInputs: Vector[OutputReference],
      outcomeSigs: Map[Sha256DigestBE, PartialSignature]): DLCAccept = {
    val pubKeys =
      DLCPublicKeys(fundingKey, toLocalCETKey, finalAddress)
    val cetSigs = CETSignatures(outcomeSigs, refundSig)
    DLCAccept(totalCollateral.satoshis,
              pubKeys,
              fundingInputs,
              changeAddress,
              cetSigs,
              eventId)
  }
}

object DLCAcceptDb {

  def fromDLCAccept(accept: DLCAccept): DLCAcceptDb = {
    DLCAcceptDb(
      accept.eventId,
      accept.pubKeys.fundingKey,
      accept.pubKeys.toLocalCETKey,
      accept.pubKeys.finalAddress,
      accept.totalCollateral,
      accept.cetSigs.refundSig,
      accept.changeAddress
    )
  }
}
