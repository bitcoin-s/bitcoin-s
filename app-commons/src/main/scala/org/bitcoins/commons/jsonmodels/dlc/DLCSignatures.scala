package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.core.protocol.script.P2WPKHWitnessV0
import org.bitcoins.core.protocol.tlv.FundingSignaturesV0TLV
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.MapWrapper
import org.bitcoins.crypto.{ECAdaptorSignature, Sha256Digest}

sealed trait DLCSignatures

case class FundingSignatures(
    sigs: Map[TransactionOutPoint, Vector[PartialSignature]])
    extends MapWrapper[TransactionOutPoint, Vector[PartialSignature]]
    with DLCSignatures {

  override protected def wrapped: Map[
    TransactionOutPoint,
    Vector[PartialSignature]] = sigs

  def merge(other: FundingSignatures): FundingSignatures = {
    val outPoints = sigs.keys ++ other.keys
    val combinedSigs = outPoints.map { outPoint =>
      val thisSigs = sigs.get(outPoint).toVector.flatten
      val otherSigs = other.get(outPoint).toVector.flatten
      outPoint -> (thisSigs ++ otherSigs)
    }

    FundingSignatures(combinedSigs.toMap)
  }

  def toTLV: FundingSignaturesV0TLV = {
    FundingSignaturesV0TLV(sigs.toVector.map {
      case (_, sigs) =>
        val PartialSignature(pubKey, sig) = sigs.head
        P2WPKHWitnessV0(pubKey, sig)
    })
  }
}

case class CETSignatures(
    outcomeSigs: Map[Sha256Digest, ECAdaptorSignature],
    refundSig: PartialSignature)
    extends DLCSignatures
