package org.bitcoins.core.util

import java.math.BigInteger

import org.bitcoins.core.number.UInt32
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

  private def runTest(nBits: UInt32, expected: BigInteger): Assertion = {
    assert(NumberUtil.targetExpansion(nBits) == expected)
  }
}
