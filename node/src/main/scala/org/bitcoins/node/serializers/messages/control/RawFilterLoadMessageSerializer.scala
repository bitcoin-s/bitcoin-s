package org.bitcoins.node.serializers.messages.control

import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.serializers.bloom.RawBloomFilterSerializer
import org.bitcoins.node.messages.FilterLoadMessage
import org.bitcoins.node.messages.control.FilterLoadMessage
import scodec.bits.ByteVector

/**
  * Created by chris on 7/19/16.
  * Serializes and deserializes a [[FilterLoadMessage]]
  * https://bitcoin.org/en/developer-reference#filterload
  */
trait RawFilterLoadMessageSerializer
    extends RawBitcoinSerializer[FilterLoadMessage] {

  override def read(bytes: ByteVector): FilterLoadMessage = {
    val filter = RawBloomFilterSerializer.read(bytes)
    FilterLoadMessage(filter)
  }

  override def write(filterLoadMessage: FilterLoadMessage): ByteVector = {
    RawBloomFilterSerializer.write(filterLoadMessage.bloomFilter)
  }
}

object RawFilterLoadMessageSerializer extends RawFilterLoadMessageSerializer
