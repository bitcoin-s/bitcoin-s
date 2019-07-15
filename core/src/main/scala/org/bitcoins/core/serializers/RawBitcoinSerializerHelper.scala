package org.bitcoins.core.serializers

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.{CompactSizeUInt, NetworkElement}
import scodec.bits.ByteVector

import scala.annotation.tailrec

/**
  * Created by chris on 2/18/16.
  */
sealed abstract class RawSerializerHelper {

  /**
    * Used parse a byte sequence to a Seq[TransactionInput], Seq[TransactionOutput], etc
    * Makes sure that we parse the correct amount of elements
    */
  def parseCmpctSizeUIntSeq[T <: NetworkElement](
      bytes: ByteVector,
      constructor: ByteVector => T): (Seq[T], ByteVector) = {
    val count = CompactSizeUInt.parse(bytes)
    val payload = bytes.splitAt(count.size.toInt)._2
    var counter = 0
    val b = Vector.newBuilder[T]
    @tailrec
    def loop(remaining: ByteVector): ByteVector = {
      if (counter == count.num.toInt) {
        remaining
      } else {
        val parsed = constructor(remaining)
        val (_, newRemaining) = remaining.splitAt(parsed.size)

        counter = counter + 1
        b.+=(parsed)

        loop(newRemaining)
      }
    }

    val remaining = loop(payload)
    val result = b.result()
    require(
      result.size == count.num.toInt,
      s"Could not parse the amount of required elements, got: ${result.size} required: ${count}")
    (result, remaining)
  }

  /** Writes a Seq[TransactionInput]/Seq[TransactionOutput]/Seq[Transaction] -> ByteVector */
  def writeCmpctSizeUInt[T](
      ts: Seq[T],
      serializer: T => ByteVector): ByteVector = {
    val serialized = write(ts, serializer)
    val cmpct = CompactSizeUInt(UInt64(ts.size))
    cmpct.bytes ++ serialized
  }

  /** Serializes a [[scala.Seq Seq]] of [[org.bitcoins.core.protocol.NetworkElement]] to a [[scodec.bits.ByteVector]] */
  def writeNetworkElements[T <: NetworkElement](ts: Seq[T]): ByteVector = {
    val f = { t: T =>
      t.bytes
    }
    write(ts, f)
  }

  def write[T](ts: Seq[T], serializer: T => ByteVector): ByteVector = {
    ts.foldLeft(ByteVector.empty) {
      case (accum, t) =>
        accum ++ serializer(t)
    }
  }
}

object RawSerializerHelper extends RawSerializerHelper
