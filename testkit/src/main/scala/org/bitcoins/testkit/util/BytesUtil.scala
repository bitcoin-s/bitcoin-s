package org.bitcoins.testkit.util

import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.{ECAdaptorSignature, ECDigitalSignature}
import org.bitcoins.dlc.testgen.DLCTestUtil
import scodec.bits.ByteVector

object BytesUtil {

  def flipAtIndex(bytes: ByteVector, byteIndex: Int): ByteVector = {
    DLCTestUtil.flipAtIndex(bytes, byteIndex)
  }

  def flipBit(signature: ECDigitalSignature): ECDigitalSignature = {
    DLCTestUtil.flipBit(signature)
  }

  def flipBit(partialSignature: PartialSignature): PartialSignature = {
    DLCTestUtil.flipBit(partialSignature)
  }

  def flipBit(adaptorSignature: ECAdaptorSignature): ECAdaptorSignature = {
    DLCTestUtil.flipBit(adaptorSignature)
  }
}
