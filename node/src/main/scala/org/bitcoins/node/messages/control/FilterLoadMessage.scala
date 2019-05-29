package org.bitcoins.node.messages.control

import org.bitcoins.core.bloom.{BloomFilter, BloomFlag}
import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.util.Factory
import org.bitcoins.node.serializers.messages.control.RawFilterLoadMessageSerializer
import org.bitcoins.node.messages
import org.bitcoins.node.serializers.messages.control.RawFilterLoadMessageSerializer
import scodec.bits.ByteVector

/**
  * @see [[https://bitcoin.org/en/developer-reference#filterload]]
  */
object FilterLoadMessage extends Factory[messages.FilterLoadMessage] {
  private case class FilterLoadMessageImpl(bloomFilter: BloomFilter)
      extends messages.FilterLoadMessage {
    require(
      bloomFilter.filterSize.num.toLong <= BloomFilter.maxSize.toLong,
      "Can only have a maximum of 36,000 bytes in our filter, got: " + bloomFilter.data.size)
    require(
      bloomFilter.hashFuncs <= BloomFilter.maxHashFuncs,
      "Can only have a maximum of 50 hashFuncs inside FilterLoadMessage, got: " + bloomFilter.hashFuncs)
    require(
      bloomFilter.filterSize.num.toLong == bloomFilter.data.size,
      "Filter Size compactSizeUInt and actual filter size were different, " +
        "filterSize: " + bloomFilter.filterSize.num + " actual filter size: " + bloomFilter.data.length
    )
  }

  override def fromBytes(bytes: ByteVector): messages.FilterLoadMessage =
    RawFilterLoadMessageSerializer.read(bytes)

  def apply(
      filterSize: CompactSizeUInt,
      filter: ByteVector,
      hashFuncs: UInt32,
      tweak: UInt32,
      flags: BloomFlag): messages.FilterLoadMessage = {
    val bloomFilter = BloomFilter(filterSize, filter, hashFuncs, tweak, flags)
    FilterLoadMessage(bloomFilter)
  }

  def apply(
      filter: ByteVector,
      hashFuncs: UInt32,
      tweak: UInt32,
      flags: BloomFlag): messages.FilterLoadMessage = {
    val filterSize = CompactSizeUInt(UInt64(filter.length))
    FilterLoadMessage(filterSize, filter, hashFuncs, tweak, flags)
  }

  def apply(bloomFilter: BloomFilter): messages.FilterLoadMessage = {
    FilterLoadMessageImpl(bloomFilter)
  }
}
