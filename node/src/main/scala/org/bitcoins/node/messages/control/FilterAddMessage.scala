package org.bitcoins.node.messages.control

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.util.Factory
import org.bitcoins.node.serializers.messages.control.RawFilterAddMessageSerializer
import org.bitcoins.node.messages
import scodec.bits.ByteVector

/**
  * Factory object for a [[FilterAddMessage]]
  * @see [[https://bitcoin.org/en/developer-reference#filteradd]]
  */
object FilterAddMessage extends Factory[messages.FilterAddMessage] {

  private case class FilterAddMessageImpl(
      elementSize: CompactSizeUInt,
      element: ByteVector)
      extends messages.FilterAddMessage

  override def fromBytes(bytes: ByteVector): messages.FilterAddMessage =
    RawFilterAddMessageSerializer.read(bytes)

  def apply(
      elementSize: CompactSizeUInt,
      element: ByteVector): messages.FilterAddMessage = {
    FilterAddMessageImpl(elementSize, element)
  }
}
