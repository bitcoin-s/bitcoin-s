package org.bitcoins.core.protocol.ln.channel

import org.bitcoins.core.number.UInt64
import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

case class ShortChannelId(u64: UInt64) extends NetworkElement {
  override def bytes: ByteVector = u64.bytes

  val blockHeight: UInt64 = (u64 >> 40) & UInt64(0xffffff)
  val txIndex: UInt64 = (u64 >> 16) & UInt64(0xffffff)
  val outputIndex: UInt64 = u64 & UInt64(0xffff)

  /** Output example:
    * {{{
    * > ShortChannelId.fromHex("db0000010000")
    * 219x1x0
    * }}}
    */
  override def toString: String = toHumanReadableString

  /** Converts the short channel id into the human readable form defined in BOLT.
    * @see [[https://github.com/lightningnetwork/lightning-rfc/blob/master/07-routing-gossip.md  BOLT7]]
    *
    * Output example:
    * {{{
    * > ShortChannelId.fromHex("db0000010000")
    * 219x1x0
    * }}}
    */
  def toHumanReadableString: String = {
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
        val txIndex = _txIndex.toInt
        val outputIndex = _outputIndex.toInt

        apply(blockHeight, txIndex, outputIndex)
      case _: Array[String] => fromHex(str)
    }

  def apply(
      blockHeight: BigInt,
      txIndex: Int,
      outputIndex: Int): ShortChannelId = {
    require(blockHeight >= 0 && blockHeight <= 0xffffff,
            s"ShortChannelId: invalid block height $blockHeight")

    require(txIndex >= 0 && txIndex <= 0xffffff,
            s"ShortChannelId:invalid tx index $txIndex")

    require(outputIndex >= 0 && outputIndex <= 0xffff,
            s"ShortChannelId: invalid output index $outputIndex")

    val u64 = UInt64(
      ((blockHeight & 0xffffffL) << 40) | ((txIndex & 0xffffffL) << 16) | (outputIndex & 0xffffL))
    ShortChannelId(u64)
  }
}
