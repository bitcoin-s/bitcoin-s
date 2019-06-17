package org.bitcoins.core.gcs

import org.bitcoins.core.number.{UInt64, UInt8}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.testkit.core.gen.NumberGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalacheck.Gen
import scodec.bits.ByteVector

class GolombFilterTest extends BitcoinSUnitTest {
  behavior of "GolombFilter"

  it must "match encoded data for arbitrary GCS parameters" in {
    def genKey: Gen[ByteVector] =
      Gen.listOfN(16, NumberGenerator.byte).map(ByteVector(_))

    def genPMRand: Gen[(UInt8, UInt64, UInt64)] = NumberGenerator.genP.flatMap {
      p =>
        // If hash's quotient when divided by 2^p is too large, we hang converting to unary
        val upperBound: Long = p.toInt * 1000 + 1

        val mGen = Gen
          .chooseNum(1L, upperBound)
          .map(UInt64(_))

        mGen.flatMap { m =>
          val upperBound = m.toInt * 2 - 2

          val randGen = Gen.chooseNum(0L, upperBound).map(UInt64(_))

          randGen.map(rand => (p, m, rand))
        }
    }

    forAll(genKey, genPMRand) {
      case (k, (p, m, rand)) =>
        val data1 = rand + UInt64.one
        val data2 = data1 + UInt64.one
        val data = Vector(data1, data2)
        val encodedData = GCS.encodeSortedSet(data, p)
        val filter =
          GolombFilter(k, m, p, CompactSizeUInt(UInt64(2)), encodedData)

        assert(!filter.matchesHash(rand))
        assert(filter.matchesHash(data1))
        assert(filter.matchesHash(data2))
    }
  }

  it must "match arbitrary encoded data for bip 158 GCS parameters" in {
    val genKey: Gen[ByteVector] =
      Gen.listOfN(16, NumberGenerator.byte).map(ByteVector(_))

    val genData: Gen[Vector[ByteVector]] = Gen.chooseNum(1, 10000).flatMap {
      size =>
        Gen.listOfN(size, NumberGenerator.bytevector).map(_.toVector)
    }

    val genRandHashes: Gen[Vector[UInt64]] =
      Gen.listOfN(1000, NumberGenerator.uInt64).map(_.toVector)

    forAll(genKey, genData, genRandHashes) {
      case (k, data, randHashes) =>
        val filter = GCS.buildBasicBlockFilter(data, k)
        val hashes = filter.decodedHashes

        data.foreach(element => assert(filter.matches(element)))

        val hashesNotInData: Vector[UInt64] =
          randHashes.filterNot(hashes.contains)

        hashesNotInData.foreach(hash => assert(!filter.matchesHash(hash)))
    }
  }
}
