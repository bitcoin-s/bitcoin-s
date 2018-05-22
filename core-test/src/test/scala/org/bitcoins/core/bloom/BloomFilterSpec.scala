package org.bitcoins.core.bloom

import org.bitcoins.core.gen.BloomFilterGenerator
import org.scalacheck.{ Prop, Properties }

/**
 * Created by chris on 8/3/16.
 */
class BloomFilterSpec extends Properties("BloomFilterSpec") {

  property("No false negatives && serialization symmetry") =
    Prop.forAll(BloomFilterGenerator.loadedBloomFilter) {
      case (loadedFilter: BloomFilter, byteVectors: Seq[scodec.bits.ByteVector]) =>
        val containsAllHashes = byteVectors.map(bytes => loadedFilter.contains(bytes))
        !containsAllHashes.exists(_ == false) &&
          BloomFilter(loadedFilter.hex) == loadedFilter
    }

}
