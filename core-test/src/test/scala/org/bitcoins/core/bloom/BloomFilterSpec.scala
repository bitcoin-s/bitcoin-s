package org.bitcoins.core.bloom

import org.bitcoins.testkit.core.gen.BloomFilterGenerator
import org.scalacheck.{Prop, Properties}
import scodec.bits.ByteVector

/**
  * Created by chris on 8/3/16.
  */
class BloomFilterSpec extends Properties("BloomFilterSpec") {

  property("No false negatives && serialization symmetry") =
    Prop.forAll(BloomFilterGenerator.loadedBloomFilter) {
      case (loadedFilter: BloomFilter, byteVectors: Seq[ByteVector]) =>
        val containsAllHashes =
          byteVectors.map(bytes => loadedFilter.contains(bytes))
        !containsAllHashes.exists(_ == false) &&
        BloomFilter(loadedFilter.hex) == loadedFilter
    }

}
