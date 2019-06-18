package org.bitcoins.core.protocol.ln

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Factory
import scodec.bits.ByteVector

case class ShortChannelId(u64: UInt64) extends NetworkElement {
  override def bytes: ByteVector = u64.bytes

  /**
    * Output example:
    * {{{
    * > ShortChannelId.fromHex("db0000010000")
    * 219x1x0
    * }}}
    */
  override def toString: String = toHumanReadableString

  /**
    * Output example:
    * {{{
    * > ShortChannelId.fromHex("db0000010000")
    * 219x1x0
    * }}}
    */
  def toHumanReadableString: String = {
    val (blockHeight, txIndex, outputIndex) =  ((u64 >> 40) & UInt64(0xFFFFFF), (u64 >> 16) & UInt64(0xFFFFFF), u64 & UInt64(0xFFFF))
    s"${blockHeight.toInt}x${txIndex.toInt}x${outputIndex.toInt}"
  }

}

object ShortChannelId extends Factory[ShortChannelId] {

  override def fromBytes(byteVector: ByteVector): ShortChannelId = {
    new ShortChannelId(UInt64.fromBytes(byteVector))
  }

  def fromHumanReadableString(str: String): ShortChannelId = str.split("x") match {
    case Array(blockHeight, txIndex, outputIndex) =>
      val u64 = UInt64(((BigInt(blockHeight) & 0xFFFFFFL) << 40) | ((txIndex.toInt & 0xFFFFFFL) << 16) | (outputIndex.toInt & 0xFFFFL))
      ShortChannelId(u64)
    case _ => fromHex(str)
  }
}
