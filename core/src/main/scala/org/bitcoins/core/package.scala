package org.bitcoins

import org.bitcoins.core.protocol.tlv.TLVPoint
import org.bitcoins.core.protocol.transaction.{
  TransactionInput,
  TransactionOutput
}
import org.bitcoins.core.wallet.fee.SatoshisPerKiloByte
import org.bitcoins.crypto.{
  CryptoOrdering,
  SchnorrDigitalSignature,
  SchnorrNonce
}
import scodec.bits._

import java.math.BigInteger
import scala.math.Ordering

package object core {

  implicit class seqUtil[T](private val seq: Seq[T]) extends AnyVal {

    def maxByOption[B](f: T => B)(implicit cmp: Ordering[B]): Option[T] = {
      if (seq.isEmpty) {
        None
      } else {
        Some(seq.maxBy(f))
      }
    }

    def minByOption[B](f: T => B)(implicit cmp: Ordering[B]): Option[T] = {
      if (seq.isEmpty) {
        None
      } else {
        Some(seq.minBy(f))
      }
    }
  }

  implicit class bigIntegerUtil(private val bigInt: BigInteger) extends AnyVal {

    implicit def intExact: Int = {
      if (bigInt.bitLength() <= 31) bigInt.intValue()
      else throw new ArithmeticException("BigInteger out of int range");
    }

    implicit def longExact: Long = {
      if (bigInt.bitLength() <= 63) bigInt.longValue()
      else throw new ArithmeticException("BigInteger out of long range");
    }
  }

  implicit val satoshisPerKiloByteOrdering: Ordering[SatoshisPerKiloByte] =
    new Ordering[SatoshisPerKiloByte] {

      override def compare(
          x: SatoshisPerKiloByte,
          y: SatoshisPerKiloByte): Int = x.toLong compare y.toLong
    }

  implicit val byteVectorOrdering: Ordering[ByteVector] =
    CryptoOrdering.byteVectorOrdering

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

  implicit val nonceOrdering: Ordering[SchnorrNonce] =
    CryptoOrdering.nonceOrdering

  implicit val schnorrSignatureOrdering: Ordering[SchnorrDigitalSignature] = {
    new Ordering[SchnorrDigitalSignature] {
      override def compare(
          x: SchnorrDigitalSignature,
          y: SchnorrDigitalSignature): Int = {
        nonceOrdering.compare(x.rx, y.rx)
      }
    }
  }

  implicit val tlvPointOrdering: Ordering[TLVPoint] = {
    new Ordering[TLVPoint] {
      override def compare(point1: TLVPoint, point2: TLVPoint): Int = {
        point1.outcome.compare(point2.outcome)
      }
    }
  }
}
