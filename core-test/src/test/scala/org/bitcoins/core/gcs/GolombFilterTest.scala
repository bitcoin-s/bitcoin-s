package org.bitcoins.core.gcs

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.crypto.{DoubleSha256Digest, SipHashKey}
import org.bitcoins.testkitcore.gen.CryptoGenerators._
import org.bitcoins.testkitcore.gen.NumberGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalacheck.Gen
import scodec.bits._

class GolombFilterTest extends BitcoinSUnitTest {
  behavior of "GolombFilter"

  it must "match encoded data for arbitrary GCS parameters" in {
    forAll(genKey, genPMRand) { case (k, (p, m, rand)) =>
      val data1 = rand + UInt64.one
      val data2 = data1 + UInt64.one
      val data = Vector(data1, data2)
      val encodedData = GCS.encodeSortedSet(data, p)
      val filter =
        GolombFilter(k, m, p, CompactSizeUInt(UInt64(2)), encodedData)
      val binarySearchMatcher = BinarySearchFilterMatcher(filter)

      assert(!binarySearchMatcher.matchesHash(rand))
      assert(binarySearchMatcher.matchesHash(data1))
      assert(binarySearchMatcher.matchesHash(data2))
      assert(!binarySearchMatcher.matchesAnyHash(Vector(rand)))
      assert(binarySearchMatcher.matchesAnyHash(Vector(rand, data1, data2)))

      val simpleMatcher = SimpleFilterMatcher(filter)

      assert(!simpleMatcher.matchesHash(rand))
      assert(simpleMatcher.matchesHash(data1))
      assert(simpleMatcher.matchesHash(data2))
      assert(!simpleMatcher.matchesAnyHash(Vector(rand)))
      assert(simpleMatcher.matchesAnyHash(Vector(rand, data1, data2)))
    }
  }

  it must "match arbitrary encoded data for bip 158 GCS parameters" in {
    assertThrows[IllegalArgumentException](SipHashKey(ByteVector.empty))

    val genKey: Gen[SipHashKey] =
      Gen
        .listOfN(16, NumberGenerator.byte)
        .map(ByteVector(_))
        .map(SipHashKey)

    val genData: Gen[Vector[ByteVector]] =
      Gen.chooseNum(1, 10000).flatMap { size =>
        Gen.listOfN(size, NumberGenerator.bytevector).map(_.toVector)
      }

    val genRandHashes: Gen[Vector[UInt64]] =
      Gen.listOfN(100, NumberGenerator.uInt64).map(_.toVector)

    forAll(genKey, genData, genRandHashes) { case (k, data, randHashes) =>
      val filter = GCS.buildBasicBlockFilter(data, k)
      val binarySearchMatcher = BinarySearchFilterMatcher(filter)
      val simpleMatcher = SimpleFilterMatcher(filter)
      val hashes = binarySearchMatcher.decodedHashes

      data.foreach(element => assert(binarySearchMatcher.matches(element)))
      assert(binarySearchMatcher.matchesAny(data))
      assert(simpleMatcher.matchesAny(data))

      val hashesNotInData: Vector[UInt64] =
        randHashes.filterNot(hashes.contains)

      assert(
        hashesNotInData.forall(hash => !binarySearchMatcher.matchesHash(hash)))
    }
  }

  it must "deserialize and serialize Golomb filters correctly" in {
    val filterBytes =
      hex"fd5b01041f9dce086df165db8ee8b1bd4f82b8de497613f464f2d47c4cc7445693ec5d011137152920fd54833157c0d4162e" ++
        hex"82d8ac631338172319d94941dc81d3eba82965b13493de62ec070f082ff1c7964970c0a7e102fa84109f01a8077009cdb72c" ++
        hex"ade1a6bd9c615daa49acccab6f63dfa49832a79e80a8c20b4c22274ef09e1ce26462610397a56926662743c8cbe2c5b402a5" ++
        hex"8d55c7f6f50fcd7d882d4c95d7660ea55453869236be30053ef9418c14873819da93c3389b3ea55950f10ce0d00034548c97" ++
        hex"94a19a200f0b8479f99720c684e04e0fd729686fa162e01bd03b8ba133ddee945695f28c5748f5555b0feb6b22da3cae3d09" ++
        hex"9f353f4508f8d12f4c4d2433c4ed230be9e7a77ce6d8c4e92397fcaa4e486627191a5c813ff0d7fe4175bddff4143c5b7cb9" ++
        hex"00e902839f659873ec7806c035d6255ddb62f49117bd1b4748305a24d75f60077f6af224cc3d2b24e1941b81873716aa6130" ++
        hex"56455e31c3053b9496039cc5d573970641d9900065f29bdbe426b78399e4986bc2b557f38273dc60d88f6463abe298d01727" ++
        hex"08753a95c2178dbf30d353f8edc44b936e0eb031a6fa6e783022234d5888e40eff13c4c9500513d90d28cbd76bb16d9f619b" ++
        hex"565138f13d43cc65ac132f77266ae88caa0916292df52554082bc735fcbd5893451290ca3f303e032d140977135f5425d113" ++
        hex"c219b109550c9502dc4dc18a9a1bd2af76099b2dfcb58f8a5b461bc9ba3f8bfdc05183573afb8c79142be80329a35754a03d" ++
        hex"aea8851779d79ba6e8165fb3c2e8feb5dc1e7cdbe8b7e240ec718cbb4802fac80f71ff00ee3bb6567c345212d1f78acc7884" ++
        hex"c6fbd249b17d7e54819c64fee00bea54270c7bd5b53cb9170836f12e078b1e2b4a63cdf45ee6201803a4664cdcdf3714e73d" ++
        hex"df33573410bdf73f6ba956b2816dcdd8aba5d5ec7021b9b1ac3a595400276d95ab553307bd47e00c9749b07b5159db0af86e" ++
        hex"4b486b81a51c32eefbe4e601fe60c2a652cc944ee0a1ad37fa4040c3fa955f88bab86b5d7d532f340cbafc5ee23d627a8f18" ++
        hex"144ac5c72e9277c612f515736c34f19658d1bd5791e163a88d33da3a57fe3c59fba315b7452588886a7c9e6b5bd86e209219" ++
        hex"59b2ec0cc8ff6d0b64b8bfea068b84285d997206bdd4127801ac5219192d53d1c4ab7bd59abb5c30c01d9056c8ee24cc639a" ++
        hex"f1379b148aaac2097c20a3ca6b5c8b2ad7c1ea693986a0cb12110dc0f19294a0ebc718f36d37b720d78decd72cd4bf7d617c" ++
        hex"38ab2411d13f90d8a1dc0a283adb014680"

    val blockHash = DoubleSha256Digest.fromHex(
      "73668ce5489ca6e42ec893ad406cc7853110ab2d63b52accf700000000000000")

    BlockFilter.fromBytes(filterBytes, blockHash).bytes must be(filterBytes)
  }

  it must "create filer header for a Golomb filter" in {
    val filterBytes = hex"017fa880"
    val blockHash = DoubleSha256Digest.fromHex(
      "73668ce5489ca6e42ec893ad406cc7853110ab2d63b52accf700000000000000")
    val filter = BlockFilter.fromBytes(filterBytes, blockHash)
    filter.bytes must be(filterBytes)

    val prevHeader = FilterHeader(
      DoubleSha256Digest.fromHex(
        "c03705b2d6fb76a59664f1d63fe8fdbb2dc076d18175fdc51d11c43afaf78a4c"),
      DoubleSha256Digest.empty)

    val header = filter.getHeader(prevHeader = prevHeader)

    val nextHeader = prevHeader.nextHeader(filter.hash)

    assert(header.prevHeaderHash == prevHeader.hash)
    assert(header == nextHeader)
    assert(nextHeader == prevHeader.nextHeader(filter))
  }
}
