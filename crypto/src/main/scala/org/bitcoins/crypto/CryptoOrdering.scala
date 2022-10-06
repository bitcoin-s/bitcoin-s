package org.bitcoins.crypto

import scodec.bits.ByteVector

object CryptoOrdering {

  val nonceOrdering: Ordering[SchnorrNonce] = {
    new Ordering[SchnorrNonce] {
      override def compare(x: SchnorrNonce, y: SchnorrNonce): Int = {
        byteVectorOrdering.compare(x.bytes, y.bytes)
      }
    }
  }

  val byteVectorOrdering: Ordering[ByteVector] =
    new Ordering[ByteVector] {

      override def compare(x: ByteVector, y: ByteVector): Int = {
        x.compare(y)
      }
    }
}
