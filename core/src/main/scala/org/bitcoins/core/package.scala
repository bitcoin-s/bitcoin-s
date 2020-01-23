package org.bitcoins

import scodec.bits._
import scala.annotation.tailrec

package object core {
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
