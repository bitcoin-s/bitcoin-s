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
    * Converts the short channel id into the human readable form defined in BOLT.
    * @see [[https://github.com/lightningnetwork/lightning-rfc/blob/master/07-routing-gossip.md  BOLT7]]
    *
    * Output example:
    * {{{
    * > ShortChannelId.fromHex("db0000010000")
    * 219x1x0
    * }}}
    */
  def toHumanReadableString: String = {
    val blockHeight = (u64 >> 40) & UInt64(0xFFFFFF)
    val txIndex = (u64 >> 16) & UInt64(0xFFFFFF)
    val outputIndex = u64 & UInt64(0xFFFF)
    s"${blockHeight.toInt}x${txIndex.toInt}x${outputIndex.toInt}"
  }

}

object ShortChannelId extends Factory[ShortChannelId] {

  override def fromBytes(byteVector: ByteVector): ShortChannelId = {
    new ShortChannelId(UInt64.fromBytes(byteVector))
  }

  def fromHumanReadableString(str: String): ShortChannelId =
    str.split("x") match {
      case Array(_blockHeight, _txIndex, _outputIndex) =>
        val blockHeight = BigInt(_blockHeight)
        require(blockHeight >= 0 && blockHeight <= 0xffffff,
                "ShortChannelId: invalid block height")

        val txIndex = _txIndex.toInt
        require(txIndex >= 0 && txIndex <= 0xffffff,
                "ShortChannelId:invalid tx index")

        val outputIndex = _outputIndex.toInt
        require(outputIndex >= 0 && outputIndex <= 0xffff,
                "ShortChannelId: invalid output index")

        val u64 = UInt64(
          ((blockHeight & 0xFFFFFFL) << 40) | ((txIndex & 0xFFFFFFL) << 16) | (outputIndex & 0xFFFFL))
        ShortChannelId(u64)
      case _: Array[String] => fromHex(str)
    }
}
