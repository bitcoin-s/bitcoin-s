package org.bitcoins.core.crypto

import scodec.bits.ByteVector

case class ECAdaptorSignature(adaptedSig: ByteVector, dleqProof: ByteVector) {
  require(adaptedSig.length == 65)
  require(dleqProof.length == 97)
}
