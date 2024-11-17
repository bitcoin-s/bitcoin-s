package org.bitcoins.testkit.util

import org.bitcoins.core.protocol.dlc.models.{CETSignatures, FundingSignatures}
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  P2WPKHWitnessV0,
  P2WSHWitnessV0,
  ScriptWitnessV0
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.{
  DigitalSignature,
  ECAdaptorSignature,
  ECDigitalSignature,
  SchnorrDigitalSignature
}
import scodec.bits.ByteVector

object BytesUtil {

  def flipAtIndex(bytes: ByteVector, byteIndex: Int): ByteVector = {
    val (front, backWithToFlip) = bytes.splitAt(byteIndex)
    val (toFlip, back) = backWithToFlip.splitAt(1)
    front ++ toFlip.xor(ByteVector.fromByte(1)) ++ back
  }

  def flipBit(signature: ECDigitalSignature): ECDigitalSignature = {
    ECDigitalSignature(flipAtIndex(signature.bytes, 60))
  }

  def flipBit(signature: SchnorrDigitalSignature): SchnorrDigitalSignature = {
    SchnorrDigitalSignature(flipAtIndex(signature.bytes, 60))
  }

  def flipBit[Sig <: DigitalSignature](
      partialSignature: PartialSignature[Sig]): PartialSignature[Sig] = {
    val s = partialSignature.signature match {
      case e: ECDigitalSignature      => flipBit(e).asInstanceOf[Sig]
      case s: SchnorrDigitalSignature => flipBit(s).asInstanceOf[Sig]
      case d: DigitalSignature =>
        sys.error(s"Cannot flip bit on unknown digital signature type=$d")
    }
    partialSignature.copy(signature = s)
  }

  def flipBit(adaptorSignature: ECAdaptorSignature): ECAdaptorSignature = {
    ECAdaptorSignature(flipAtIndex(adaptorSignature.bytes, 40))
  }

  def flipBit(witness: ScriptWitnessV0): ScriptWitnessV0 = {
    witness match {
      case p2wpkh: P2WPKHWitnessV0 =>
        P2WPKHWitnessV0(p2wpkh.pubKey, flipBit(p2wpkh.signature))
      case p2wsh: P2WSHWitnessV0 =>
        val sigOpt = p2wsh.stack.zipWithIndex.find { case (bytes, _) =>
          bytes.length >= 67 && bytes.length <= 73
        }

        sigOpt match {
          case Some((sig, index)) =>
            P2WSHWitnessV0(
              EmptyScriptPubKey,
              p2wsh.stack.updated(index, flipBit(ECDigitalSignature(sig)).bytes)
            )
          case None =>
            P2WSHWitnessV0(
              EmptyScriptPubKey,
              p2wsh.stack.updated(0, flipAtIndex(p2wsh.stack.head, 0))
            )
        }
    }
  }

  def flipBit(fundingSigs: FundingSignatures): FundingSignatures = {
    val (firstOutPoint, witness) = fundingSigs.head
    val badWitness = flipBit(witness)
    FundingSignatures(fundingSigs.tail.toVector.+:(firstOutPoint -> badWitness))
  }

  def flipBit(
      cetSigs: CETSignatures,
      refundSig: PartialSignature[ECDigitalSignature]
  ): (CETSignatures, PartialSignature[ECDigitalSignature]) = {
    val badOutcomeSigs = cetSigs.outcomeSigs.map { case (outcome, sig) =>
      outcome -> flipBit(sig)
    }
    val badRefundSig = flipBit(refundSig)
    (CETSignatures(badOutcomeSigs), badRefundSig)
  }
}
