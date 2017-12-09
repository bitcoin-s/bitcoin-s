package org.bitcoins.core.serializers.bloom

import org.bitcoins.core.bloom.{BloomFilter, BloomFlag}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.BitcoinSUtil

/**
  * Created by chris on 8/4/16.
  * [[https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki#new-messages]]
  */
trait RawBloomFilterSerializer extends RawBitcoinSerializer[BloomFilter] {

  override def read(bytes: List[Byte]): BloomFilter = {
    val filterSize = CompactSizeUInt.parseCompactSizeUInt(bytes)
    val filter = bytes.slice(filterSize.size.toInt, filterSize.size.toInt + filterSize.num.toInt)
    val hashFuncsIndex = (filterSize.size + filterSize.num.toInt).toInt
    val hashFuncs = UInt32(BitcoinSUtil.flipEndianness(bytes.slice(hashFuncsIndex,hashFuncsIndex + 4)))
    val tweakIndex = hashFuncsIndex + 4
    val tweak = UInt32(BitcoinSUtil.flipEndianness(bytes.slice(tweakIndex, tweakIndex + 4)))
    val flags = BloomFlag(bytes(tweakIndex+4))
    BloomFilter(filterSize,filter,hashFuncs,tweak,flags)

  }

  override def write(bloomFilter: BloomFilter): String = {
    bloomFilter.filterSize.hex + BitcoinSUtil.encodeHex(bloomFilter.data) +
      BitcoinSUtil.flipEndianness(bloomFilter.hashFuncs.hex) +
      BitcoinSUtil.flipEndianness(bloomFilter.tweak.hex) +
      BitcoinSUtil.encodeHex(bloomFilter.flags.byte)
  }
}

object RawBloomFilterSerializer extends RawBloomFilterSerializer