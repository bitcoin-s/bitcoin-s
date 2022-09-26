package org.bitcoins.crypto

import scodec.bits.ByteVector

import scala.annotation.tailrec

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

      @tailrec
      override def compare(x: ByteVector, y: ByteVector): Int = {
        if (x == y) {
          0
        } else if (x.isEmpty) {
          -1
        } else if (y.isEmpty) {
          1
        } else if (x.head != y.head) {
          x.head.compare(y.head)
        } else {
          compare(x.tail, y.tail)
        }
      }
    }
}
