package org.bitcoins.core.util

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.crypto.{CryptoBytesUtil, Factory, NetworkElement}
import scodec.bits.ByteVector

import scala.annotation.tailrec

trait BytesUtil extends CryptoBytesUtil {

  def writeCmpctSizeUInt[T <: NetworkElement](ts: Seq[T]): ByteVector = {
    val serialized = ts.map(_.bytes).foldLeft(ByteVector.empty)(_ ++ _)
    val cmpct = CompactSizeUInt(UInt64(ts.size))
    cmpct.bytes ++ serialized
  }

  /**
    * Used parse a byte sequence to a Seq[TransactionInput], Seq[TransactionOutput], etc
    * Makes sure that we parse the correct amount of elements
    */
  def parseCmpctSizeUIntSeq[T <: NetworkElement](
      bytes: ByteVector,
      factory: Factory[T]): (Vector[T], ByteVector) = {
    val count = CompactSizeUInt.parse(bytes)
    val payload = bytes.drop(count.byteSize.toInt)
    val builder = Vector.newBuilder[T]

    @tailrec
    def loop(remaining: ByteVector, counter: Int = 0): ByteVector = {
      if (counter == count.num.toInt) {
        remaining
      } else {
        val parsed = factory.fromBytes(remaining)
        val newRemaining = remaining.drop(parsed.byteSize)

        builder.+=(parsed)

        loop(newRemaining, counter + 1)
      }
    }

    val remaining = loop(payload)
    val result = builder.result()
    require(
      result.size == count.num.toInt,
      s"Could not parse the amount of required elements, got: ${result.size} required: ${count}")

    (result, remaining)
  }
}

object BytesUtil extends BytesUtil
