package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.serializers.bloom.RawBloomFilterSerializer
import org.bitcoins.core.p2p.FilterLoadMessage
import scodec.bits.ByteVector

/**
  * Serializes and deserializes a [[org.bitcoins.core.p2p.FilterLoadMessage FilterLoadMessage]]
  * @see [[https://bitcoin.org/en/developer-reference#filterload]]
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
