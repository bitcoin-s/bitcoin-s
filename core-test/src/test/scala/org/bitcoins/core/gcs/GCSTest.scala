package org.bitcoins.core.gcs

import org.bitcoins.core.number.{UInt64, UInt8}
import org.bitcoins.core.util.NumberUtil
import org.bitcoins.crypto.SipHashKey
import org.bitcoins.testkitcore.gen.NumberGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalacheck.Gen
import scodec.bits.{BinStringSyntax, ByteVector}

class GCSTest extends BitcoinSUnitTest {
  behavior of "GCS"

  //https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#golomb-rice-coding
  it must "encode and decode Golomb Coded Set example 1" in {
    val p = UInt8(2)
    val original = UInt64.zero

    val encoding = GCS.golombEncode(item = original, p = p)

    assert(encoding == bin"000")

    val decode = GCS.golombDecode(codedItem = encoding, p = p)

    assert(decode == original)
  }

  it must "encode and decode Golomb Coded set example 2" in {
    val p = UInt8(2)
    val original = UInt64.one

    val encoding = GCS.golombEncode(item = original, p = p)

    assert(encoding == bin"001")

    val decode = GCS.golombDecode(codedItem = encoding, p = p)

    assert(decode == original)
  }

  it must "encode and decode Golomb Coded set example 3" in {
    val p = UInt8(2)
    val original = UInt64(2)

    val encoding = GCS.golombEncode(item = original, p = p)

    assert(encoding == bin"010")

    val decode = GCS.golombDecode(codedItem = encoding, p = p)

    assert(decode == original)
  }

  it must "encode and decode Golomb Coded set example 4" in {
    val p = UInt8(2)
    val original = UInt64(3)

    val encoding = GCS.golombEncode(item = original, p = p)

    assert(encoding == bin"011")

    val decode = GCS.golombDecode(codedItem = encoding, p = p)

    assert(decode == original)
  }

  it must "encode and decode Golomb Coded set example 5" in {
    val p = UInt8(2)
    val original = UInt64(4)

    val encoding = GCS.golombEncode(item = original, p = p)

    assert(encoding == bin"1000")

    val decode = GCS.golombDecode(codedItem = encoding, p = p)

    assert(decode == original)
  }

  it must "encode and decode Golomb Coded set example 6" in {
    val p = UInt8(2)
    val original = UInt64(5)

    val encoding = GCS.golombEncode(item = original, p = p)

    assert(encoding == bin"1001")

    val decode = GCS.golombDecode(codedItem = encoding, p = p)

    assert(decode == original)
  }

  it must "encode and decode Golomb Coded set example 7" in {
    val p = UInt8(2)
    val original = UInt64(6)

    val encoding = GCS.golombEncode(item = original, p = p)

    assert(encoding == bin"1010")

    val decode = GCS.golombDecode(codedItem = encoding, p = p)

    assert(decode == original)
  }

  it must "encode and decode Golomb Coded set example 8" in {
    val p = UInt8(2)
    val original = UInt64(7)

    val encoding = GCS.golombEncode(item = original, p = p)

    assert(encoding == bin"1011")

    val decode = GCS.golombDecode(codedItem = encoding, p = p)

    assert(decode == original)
  }

  it must "encode and decode Golomb Coded set example 9" in {
    val p = UInt8(2)
    val original = UInt64(8)

    val encoding = GCS.golombEncode(item = original, p = p)

    assert(encoding == bin"11000")

    val decode = GCS.golombDecode(codedItem = encoding, p = p)

    assert(decode == original)
  }

  it must "encode and decode Golomb Coded set example 10" in {
    val p = UInt8(2)
    val original = UInt64(9)

    val encoding = GCS.golombEncode(item = original, p = p)

    assert(encoding == bin"11001")

    val decode = GCS.golombDecode(codedItem = encoding, p = p)

    assert(decode == original)
  }

  it must "encode and decode an arbitrary item for an arbitrary p" in {

    def delta: Gen[UInt64] = {
      //what is a reasonable delta? This is means the delta
      //can be 1 - 16384
      //if we do a full uint64 it takes forever to encode it
      Gen
        .choose(1, NumberUtil.pow2(14).toInt)
        .map(UInt64(_))
    }

    forAll(delta, NumberGenerator.genP) { case (item, p) =>
      val encoded = GCS.golombEncode(item = item, p = p)
      val decode = GCS.golombDecode(codedItem = encoded, p = p)

      assert(decode == item)
    }
  }

  it must "encode and decode a set of elements already tested" in {
    val p = UInt8(2)

    // Diffs are 1, 2, 3, 4, 5
    val sortedItems =
      Vector(UInt64(0), UInt64(1), UInt64(3), UInt64(6), UInt64(10), UInt64(15))

    val codedSet = GCS.encodeSortedSet(sortedItems, p)

    val coded0 = bin"000"
    val coded1 = bin"001"
    val coded2 = bin"010"
    val coded3 = bin"011"
    val coded4 = bin"1000"
    val coded5 = bin"1001"
    val expectedCodedSet =
      coded0 ++ coded1 ++ coded2 ++ coded3 ++ coded4 ++ coded5

    assert(codedSet == expectedCodedSet)

    val decodedSet = GCS.golombDecodeSet(codedSet, p)

    assert(decodedSet == sortedItems)
  }

  it must "encode and decode arbitrary ByteVectors for arbitrary p" in {

    def genP: Gen[UInt8] = {
      // We have 8 as a lower bound since N in hashToRange is in the order of 1000
      Gen.choose(8, 32).map(UInt8(_))
    }

    def genPM: Gen[(UInt8, UInt64)] =
      genP.flatMap { p =>
        // If hash's quotient when divided by 2^p is too large, we hang converting to unary
        val upperBound: Long = p.toInt * 1000

        val mGen = Gen
          .chooseNum(0L, upperBound)
          .map(UInt64(_))

        mGen.map(m => (p, m))
      }

    def genItems: Gen[Vector[ByteVector]] = {
      Gen.choose(1, 1000).flatMap { size =>
        Gen.listOfN(size, NumberGenerator.bytevector).map(_.toVector)
      }
    }

    def genKey: Gen[SipHashKey] =
      Gen
        .listOfN(16, NumberGenerator.byte)
        .map(ByteVector(_))
        .map(SipHashKey(_))

    forAll(genPM, genItems, genKey) { case ((p, m), items, k) =>
      val hashes = GCS.hashedSetConstruct(items, k, m)
      val sortedHashes = hashes.sortWith(_ < _)

      val codedSet = GCS.buildGCS(items, k, p, m)
      val decodedSet = GCS.golombDecodeSet(codedSet, p)

      assert(decodedSet == sortedHashes)
    }

  }
}
