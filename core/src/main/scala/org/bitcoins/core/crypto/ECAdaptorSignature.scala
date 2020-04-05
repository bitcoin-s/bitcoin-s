package org.bitcoins.core.crypto

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Factory
import scodec.bits.ByteVector

case class ECAdaptorSignature(adaptedSig: ByteVector, dleqProof: ByteVector)
    extends NetworkElement {
  require(adaptedSig.length == 65)
  require(dleqProof.length == 97)

  def bytes: ByteVector = adaptedSig ++ dleqProof
}

object ECAdaptorSignature extends Factory[ECAdaptorSignature] {

  def fromBytes(bytes: ByteVector): ECAdaptorSignature = {
    require(bytes.length == 65 + 97)
    ECAdaptorSignature(bytes.take(65), bytes.drop(65))
  }
}
