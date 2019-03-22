package org.bitcoins.core.util

import java.math.BigInteger

import org.bitcoins.core.number.UInt32
import org.bitcoins.testkit.core.gen.NumberGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalatest.Assertion
import scodec.bits.ByteVector

class NumberUtilTest extends BitcoinSUnitTest {

  behavior of "NumberUtil"

  it must "expand nbits to 0 difficulty threshold" in {

    //from the examples table on bitcoin developer reference site
    //https://bitcoin.org/en/developer-reference#target-nbits
    val nBits1 = UInt32.fromHex("01003456")
    val expected1 = BigInteger.valueOf(0)

    runTest(nBits1, expected1)

    val nBits2 = UInt32.fromHex("01123456")
    val expected2 = BigInteger.valueOf(18)

    runTest(nBits2, expected2)

    val nBits3 = UInt32.fromHex("02008000")
    val expected3 = BigInteger.valueOf(128)

    runTest(nBits3, expected3)

    val nBits4 = UInt32.fromHex("05009234")
    val expected4 = BigInteger.valueOf(2452881408L)

    runTest(nBits4, expected4)

    val nBits6 = UInt32.fromHex("04123456")
    val expected6 = BigInteger.valueOf(305419776)

    runTest(nBits6, expected6)

    val nBits5 = UInt32.fromHex("04923456")
    val expected5 = BigInteger.valueOf(-305419776)

    runTest(nBits5, expected5)
  }

  it must "expand the minimum difficulty on bitcoin main network" in {
    //https://stackoverflow.com/questions/22059359/trying-to-understand-nbits-value-from-stratum-protocol
    val nBits = UInt32.fromHex("1d00ffff")
    val expected = new BigInteger(
      "00ffff0000000000000000000000000000000000000000000000000000",
      16)

    runTest(nBits, expected)
  }

  it must "expand the minimum difficulty correctly on bitcoin regtest" in {
    val nBits = UInt32.fromHex("207fffff")
    val expected = new BigInteger(
      "57896037716911750921221705069588091649609539881711309849342236841432341020672",
      10
    )

    runTest(nBits, expected)
  }

  behavior of "NumberUtil.targetCompression"

  it must "handle all cases as enumerated in bitcoin core" in {
    //https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/test/arith_uint256_tests.cpp#L405
    val expanded = NumberUtil.targetExpansion(UInt32.zero)
    NumberUtil.targetCompression(expanded, false) must be(UInt32.zero)

    val expanded1 = NumberUtil.targetExpansion(UInt32.fromHex("00123456"))
    NumberUtil.targetCompression(expanded1, false) must be(UInt32.zero)

    val expanded2 = NumberUtil.targetExpansion(UInt32.fromHex("01003456"))
    NumberUtil.targetCompression(expanded2, false) must be(UInt32.zero)

    val expanded3 = NumberUtil.targetExpansion(UInt32.fromHex("02000056"))
    NumberUtil.targetCompression(expanded3, false) must be(UInt32.zero)

    val expanded4 = NumberUtil.targetExpansion(UInt32.fromHex("03000000"))
    NumberUtil.targetCompression(expanded4, false) must be(UInt32.zero)

    val expanded5 = NumberUtil.targetExpansion(UInt32.fromHex("04000000"))
    NumberUtil.targetCompression(expanded5, false) must be(UInt32.zero)

    val expanded6 = NumberUtil.targetExpansion(UInt32.fromHex("01803456"))

    NumberUtil.targetCompression(expanded6, false) must be(UInt32.zero)

    val expanded7 = NumberUtil.targetExpansion(UInt32.fromHex("02800056"))

    NumberUtil.targetCompression(expanded7, isNegative = false) must be(
      UInt32.zero)

    val expanded8 = NumberUtil.targetExpansion(UInt32.fromHex("03800000"))

    NumberUtil.targetCompression(expanded8, false) must be(UInt32.zero)

    val expanded9 = NumberUtil.targetExpansion(UInt32.fromHex("04800000"))

    NumberUtil.targetCompression(expanded9, false) must be(UInt32.zero)

    val expanded10 = NumberUtil.targetExpansion(UInt32.fromHex("01123456"))

    NumberUtil.targetCompression(expanded10, false) must be(
      UInt32.fromHex("01120000"))

    NumberUtil.targetCompression(BigInt(0x80).bigInteger, false) must be(
      UInt32.fromHex("02008000"))

    //ignoring this negative test case for now
    //https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/test/arith_uint256_tests.cpp#L486

    val expanded12 = NumberUtil.targetExpansion(UInt32.fromHex("02123456"))
    NumberUtil.targetCompression(expanded12, false) must be(
      UInt32.fromHex("02123400"))

    val expanded13 = NumberUtil.targetExpansion(UInt32.fromHex("03123456"))
    NumberUtil.targetCompression(expanded13, false) must be(
      UInt32.fromHex("03123456"))

    val expanded14 = NumberUtil.targetExpansion(UInt32.fromHex("04123456"))
    NumberUtil.targetCompression(expanded14, false) must be(
      UInt32.fromHex("04123456"))

    //skipping this negative test case for now
    //https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/test/arith_uint256_tests.cpp#L510

    val expanded15 = NumberUtil.targetExpansion(UInt32.fromHex("05009234"))
    NumberUtil.targetCompression(expanded15, false) must be(
      UInt32.fromHex("05009234"))

    val expanded16 = NumberUtil.targetExpansion(UInt32.fromHex("20123456"))
    NumberUtil.targetCompression(expanded16, false) must be(
      UInt32.fromHex("20123456"))

    //skipping overflow test case for now
    //https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/test/arith_uint256_tests.cpp#L528
  }

  private def runTest(nBits: UInt32, expected: BigInteger): Assertion = {
    val expansion = NumberUtil.targetExpansion(nBits)
    assert(expansion == expected)
  }
}
