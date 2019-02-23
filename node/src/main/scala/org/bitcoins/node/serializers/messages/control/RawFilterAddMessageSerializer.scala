package org.bitcoins.node.serializers.messages.control

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.node.messages.FilterAddMessage
import org.bitcoins.node.messages.control.FilterAddMessage
import org.bitcoins.node.messages.FilterAddMessage
import org.bitcoins.node.messages.control.FilterAddMessage
import scodec.bits.ByteVector

/**
  * Created by chris on 8/26/16.
  * Responsible for serializing and deserializing a [[FilterAddMessage]]
  * [[https://bitcoin.org/en/developer-reference#filteradd]]
  */
trait RawFilterAddMessageSerializer
    extends RawBitcoinSerializer[FilterAddMessage] {

  override def read(bytes: ByteVector): FilterAddMessage = {
    val elementSize = CompactSizeUInt.parseCompactSizeUInt(bytes)
    val element = bytes.slice(elementSize.size.toInt, bytes.size)
    FilterAddMessage(elementSize, element)
  }

  override def write(filterAddMessage: FilterAddMessage): ByteVector = {
    filterAddMessage.elementSize.bytes ++ filterAddMessage.element
  }
}

object RawFilterAddMessageSerializer extends RawFilterAddMessageSerializer
