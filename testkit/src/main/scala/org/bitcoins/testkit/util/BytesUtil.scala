package org.bitcoins.testkit.util

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
}
