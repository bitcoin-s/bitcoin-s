package org.bitcoins.testkit.util

import org.bitcoins.core.protocol.dlc.{CETSignatures, FundingSignatures}
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  P2WPKHWitnessV0,
  P2WSHWitnessV0,
  ScriptWitnessV0
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.{ECAdaptorSignature, ECDigitalSignature}
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

  def flipBit(partialSignature: PartialSignature): PartialSignature = {
    partialSignature.copy(signature = flipBit(partialSignature.signature))
  }

  def flipBit(adaptorSignature: ECAdaptorSignature): ECAdaptorSignature = {
    ECAdaptorSignature(flipAtIndex(adaptorSignature.bytes, 40))
  }

  def flipBit(witness: ScriptWitnessV0): ScriptWitnessV0 = {
    witness match {
      case p2wpkh: P2WPKHWitnessV0 =>
        P2WPKHWitnessV0(p2wpkh.pubKey, flipBit(p2wpkh.signature))
      case p2wsh: P2WSHWitnessV0 =>
        val sigOpt = p2wsh.stack.zipWithIndex.find {
          case (bytes, _) =>
            bytes.length >= 67 && bytes.length <= 73
        }

        sigOpt match {
          case Some((sig, index)) =>
            P2WSHWitnessV0(
              EmptyScriptPubKey,
              p2wsh.stack.updated(index,
                                  flipBit(ECDigitalSignature(sig)).bytes))
          case None =>
            P2WSHWitnessV0(
              EmptyScriptPubKey,
              p2wsh.stack.updated(0, flipAtIndex(p2wsh.stack.head, 0)))
        }
    }
  }

  def flipBit(fundingSigs: FundingSignatures): FundingSignatures = {
    val (firstOutPoint, witness) = fundingSigs.head
    val badWitness = flipBit(witness)
    FundingSignatures(fundingSigs.tail.toVector.+:(firstOutPoint -> badWitness))
  }

  def flipBit(cetSigs: CETSignatures): CETSignatures = {
    val badOutcomeSigs = cetSigs.outcomeSigs.map {
      case (outcome, sig) => outcome -> flipBit(sig)
    }
    val badRefundSig = flipBit(cetSigs.refundSig)
    CETSignatures(badOutcomeSigs, badRefundSig)
  }
}
