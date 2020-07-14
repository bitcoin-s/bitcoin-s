package org.bitcoins

import org.bitcoins.core.wallet.fee.SatoshisPerKiloByte
import scodec.bits._

import scala.annotation.tailrec

package object core {

  implicit val satoshisPerKiloByteOrdering: Ordering[SatoshisPerKiloByte] =
    new Ordering[SatoshisPerKiloByte] {

      override def compare(
          x: SatoshisPerKiloByte,
          y: SatoshisPerKiloByte): Int = x.toLong compare y.toLong
    }

  implicit val byteVectorOrdering: Ordering[ByteVector] =
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
