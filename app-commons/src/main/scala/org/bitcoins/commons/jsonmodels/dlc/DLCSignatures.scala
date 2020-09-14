package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.core.protocol.tlv.FundingSignaturesTempTLV
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

  def toTLV: FundingSignaturesTempTLV = {
    FundingSignaturesTempTLV(sigs.map {
      case (outPoint, sigs) => (outPoint, sigs.head.signature)
    })
  }
}

case class CETSignatures(
    outcomeSigs: Map[Sha256Digest, ECAdaptorSignature],
    refundSig: PartialSignature)
    extends DLCSignatures
