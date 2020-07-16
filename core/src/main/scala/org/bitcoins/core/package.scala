package org.bitcoins

import org.bitcoins.core.protocol.transaction.{
  TransactionInput,
  TransactionOutput
}
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

  implicit val transactionInputOrder: Ordering[TransactionInput] =
    new Ordering[TransactionInput] {

      override def compare(x: TransactionInput, y: TransactionInput): Int =
        x.previousOutput.compare(y.previousOutput)
    }

  implicit val transactionOutputOrder: Ordering[TransactionOutput] =
    new Ordering[TransactionOutput] {

      override def compare(x: TransactionOutput, y: TransactionOutput): Int =
        if (x.value == y.value) {
          x.scriptPubKey.hex.compare(y.scriptPubKey.hex)
        } else x.value.compare(y.value)
    }
}
