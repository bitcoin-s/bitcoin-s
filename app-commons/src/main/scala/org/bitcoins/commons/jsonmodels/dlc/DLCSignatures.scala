package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.core.protocol.script.P2WPKHWitnessV0
import org.bitcoins.core.protocol.tlv.FundingSignaturesV0TLV
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.SeqWrapper
import org.bitcoins.crypto.{ECAdaptorSignature, Sha256Digest}

sealed trait DLCSignatures

case class FundingSignatures(
    sigs: Vector[(TransactionOutPoint, Vector[PartialSignature])])
    extends SeqWrapper[(TransactionOutPoint, Vector[PartialSignature])]
    with DLCSignatures {

  override protected def wrapped: Vector[
    (TransactionOutPoint, Vector[PartialSignature])] = sigs

  def get(outPoint: TransactionOutPoint): Option[Vector[PartialSignature]] = {
    sigs.find(_._1 == outPoint).map(_._2)
  }

  def apply(outPoint: TransactionOutPoint): Vector[PartialSignature] = {
    get(outPoint).get
  }

  /** Note that this function does not preserver order */
  def merge(other: FundingSignatures): FundingSignatures = {
    val local = sigs.toMap
    val remote = other.toMap

    val outPoints = local.keys ++ remote.keys
    val combinedSigs = outPoints.map { outPoint =>
      val thisSigs = local.get(outPoint).toVector.flatten
      val remoteSigs = remote.get(outPoint).toVector.flatten
      outPoint -> (thisSigs ++ remoteSigs)
    }

    FundingSignatures(combinedSigs.toVector)
  }

  def toTLV: FundingSignaturesV0TLV = {
    FundingSignaturesV0TLV(sigs.map {
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
