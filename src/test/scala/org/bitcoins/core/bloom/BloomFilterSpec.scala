package org.bitcoins.core.bloom

import org.bitcoins.core.gen.BloomFilterGenerator
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 8/3/16.
  */
class BloomFilterSpec extends Properties("BloomFilterSpec") {

  property("Serialization symmetry") =
    Prop.forAll(BloomFilterGenerator.bloomFilter) { filter =>
      BloomFilter(filter.hex) == filter
    }

  property("No false negatives") =
    Prop.forAll(BloomFilterGenerator.loadedBloomFilter) {
      case (loadedFilter: BloomFilter, byteVectors: Seq[Seq[Byte]]) =>
        val containsAllHashes = byteVectors.map(bytes => loadedFilter.contains(bytes))
        !containsAllHashes.exists(_ == false)
    }

}
