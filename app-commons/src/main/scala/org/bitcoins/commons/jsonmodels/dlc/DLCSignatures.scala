package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.MapWrapper
import org.bitcoins.crypto.Sha256DigestBE

sealed trait DLCSignatures

case class FundingSignatures(
    sigs: Map[TransactionOutPoint, Vector[PartialSignature]])
    extends MapWrapper[TransactionOutPoint, Vector[PartialSignature]]
    with DLCSignatures {

  override protected def wrapped: Map[
    TransactionOutPoint,
    Vector[PartialSignature]] = sigs
}

case class CETSignatures(
    outcomeSigs: Map[Sha256DigestBE, PartialSignature],
    refundSig: PartialSignature)
    extends DLCSignatures
