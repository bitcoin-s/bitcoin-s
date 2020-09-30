package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.core.protocol.script.ScriptWitnessV0
import org.bitcoins.core.protocol.tlv.FundingSignaturesV0TLV
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.SeqWrapper
import org.bitcoins.crypto.{ECAdaptorSignature, Sha256Digest}

sealed trait DLCSignatures

case class FundingSignatures(
    sigs: Vector[(TransactionOutPoint, ScriptWitnessV0)])
    extends SeqWrapper[(TransactionOutPoint, ScriptWitnessV0)]
    with DLCSignatures {

  override protected def wrapped: Vector[
    (TransactionOutPoint, ScriptWitnessV0)] = sigs

  def get(outPoint: TransactionOutPoint): Option[ScriptWitnessV0] = {
    sigs.find(_._1 == outPoint).map(_._2)
  }

  def apply(outPoint: TransactionOutPoint): ScriptWitnessV0 = {
    get(outPoint).get
  }

  def merge(other: FundingSignatures): FundingSignatures = {
    FundingSignatures(sigs ++ other.sigs)
  }

  def toTLV: FundingSignaturesV0TLV = {
    FundingSignaturesV0TLV(sigs.map(_._2))
  }
}

case class CETSignatures(
    outcomeSigs: Map[Sha256Digest, ECAdaptorSignature],
    refundSig: PartialSignature)
    extends DLCSignatures
