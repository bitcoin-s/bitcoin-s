package org.bitcoins.core.util

import java.math.BigInteger

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalatest.Assertion

class NumberUtilTest extends BitcoinSUnitTest {

  behavior of "NumberUtil"

  it must "expand nbits to 0 difficulty threshold" in {

    //from the examples table on bitcoin developer reference site
    //https://bitcoin.org/en/developer-reference#target-nbits
    val nBits1 = UInt32.fromHex("01003456")
    val expected1 = BigInteger.valueOf(0)
    val diffHelper1 = {
      BlockHeader.TargetDifficultyHelper(
        difficulty = expected1,
        isNegative = false,
        isOverflow = false
      )
    }
    runTest(nBits1, diffHelper1)

    val nBits2 = UInt32.fromHex("01123456")
    val expected2 = BigInteger.valueOf(18)
    val diffHelper2 = {
      BlockHeader.TargetDifficultyHelper(
        difficulty = expected2,
        isNegative = false,
        isOverflow = false
      )
    }
    runTest(nBits2, diffHelper2)

    val nBits3 = UInt32.fromHex("02008000")
    val expected3 = BigInteger.valueOf(128)
    val diffHelper3 = {
      BlockHeader.TargetDifficultyHelper(
        difficulty = expected3,
        isNegative = false,
        isOverflow = false
      )
    }
    runTest(nBits3, diffHelper3)

    val nBits4 = UInt32.fromHex("05009234")
    val expected4 = BigInteger.valueOf(2452881408L)
    val diffHelper4 = {
      BlockHeader.TargetDifficultyHelper(
        difficulty = expected4,
        isNegative = false,
        isOverflow = false
      )
    }
    runTest(nBits4, diffHelper4)

    val nBits6 = UInt32.fromHex("04123456")
    val expected6 = BigInteger.valueOf(305419776)
    val diffHelper6 = {
      BlockHeader.TargetDifficultyHelper(
        difficulty = expected6,
        isNegative = false,
        isOverflow = false
      )
    }
    runTest(nBits6, diffHelper6)

    val nBits5 = UInt32.fromHex("04923456")
    val expected5 = BigInteger.valueOf(305419776)
    val diffHelper5 = {
      BlockHeader.TargetDifficultyHelper(
        difficulty = expected5,
        isNegative = true,
        isOverflow = false
      )
    }
    runTest(nBits5, diffHelper5)
  }

  it must "expand the minimum difficulty on bitcoin main network" in {
    //https://stackoverflow.com/questions/22059359/trying-to-understand-nbits-value-from-stratum-protocol
    val nBits = UInt32.fromHex("1d00ffff")
    val expected = new BigInteger(
      "00ffff0000000000000000000000000000000000000000000000000000",
      16)
    val diffHelper = {
      BlockHeader.TargetDifficultyHelper(
        expected,
        false,
        false
      )
    }
    runTest(nBits, diffHelper)
  }

  it must "expand the minimum difficulty correctly on bitcoin regtest" in {
    val nBits = UInt32.fromHex("207fffff")
    val expected = new BigInteger(
      "57896037716911750921221705069588091649609539881711309849342236841432341020672",
      10
    )

    val diffHelper = {
      BlockHeader.TargetDifficultyHelper(
        expected,
        false,
        false
      )
    }

    runTest(nBits, diffHelper)
  }

  behavior of "NumberUtil.targetCompression"

  it must "handle all cases as enumerated in bitcoin core" in {
    //https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/test/arith_uint256_tests.cpp#L405
    val expanded = NumberUtil.targetExpansion(UInt32.zero)
    NumberUtil.targetCompression(expanded) must be(UInt32.zero)

    val expanded1 = NumberUtil.targetExpansion(UInt32.fromHex("00123456"))
    NumberUtil.targetCompression(expanded1) must be(UInt32.zero)

    val expanded2 = NumberUtil.targetExpansion(UInt32.fromHex("01003456"))
    NumberUtil.targetCompression(expanded2) must be(UInt32.zero)

    val expanded3 = NumberUtil.targetExpansion(UInt32.fromHex("02000056"))
    NumberUtil.targetCompression(expanded3) must be(UInt32.zero)

    val expanded4 = NumberUtil.targetExpansion(UInt32.fromHex("03000000"))
    NumberUtil.targetCompression(expanded4) must be(UInt32.zero)

    val expanded5 = NumberUtil.targetExpansion(UInt32.fromHex("04000000"))
    NumberUtil.targetCompression(expanded5) must be(UInt32.zero)

    val expanded6 = NumberUtil.targetExpansion(UInt32.fromHex("01803456"))

    NumberUtil.targetCompression(expanded6) must be(UInt32.zero)

    val expanded7 = NumberUtil.targetExpansion(UInt32.fromHex("02800056"))

    NumberUtil.targetCompression(expanded7) must be(UInt32.zero)

    val expanded8 = NumberUtil.targetExpansion(UInt32.fromHex("03800000"))

    NumberUtil.targetCompression(expanded8) must be(UInt32.zero)

    val expanded9 = NumberUtil.targetExpansion(UInt32.fromHex("04800000"))

    NumberUtil.targetCompression(expanded9) must be(UInt32.zero)

    val expanded10 = NumberUtil.targetExpansion(UInt32.fromHex("01123456"))

    NumberUtil.targetCompression(expanded10) must be(UInt32.fromHex("01120000"))

    NumberUtil.targetCompression(bigInt = BigInt(0x80),
                                 isNegative = false) must be(
      UInt32.fromHex("02008000"))
    val expanded11 = NumberUtil.targetExpansion(UInt32.fromHex("01fedcba"))

    expanded11.difficulty must be(126)
    expanded11.isNegative must be(true)
    NumberUtil.targetCompression(expanded11) must be(UInt32.fromHex("01fe0000"))

    NumberUtil.targetCompression(bigInt = BigInt(0x80),
                                 isNegative = false) must be(
      UInt32.fromHex("02008000"))

    val expanded12 = NumberUtil.targetExpansion(UInt32.fromHex("02123456"))
    NumberUtil.targetCompression(expanded12) must be(UInt32.fromHex("02123400"))

    val expanded13 = NumberUtil.targetExpansion(UInt32.fromHex("03123456"))
    NumberUtil.targetCompression(expanded13) must be(UInt32.fromHex("03123456"))

    val expanded14 = NumberUtil.targetExpansion(UInt32.fromHex("04123456"))
    NumberUtil.targetCompression(expanded14) must be(UInt32.fromHex("04123456"))

    val expanded15 = NumberUtil.targetExpansion(UInt32.fromHex("04923456"))
    NumberUtil.targetCompression(expanded15) must be(UInt32.fromHex("04923456"))

    val expanded16 = NumberUtil.targetExpansion(UInt32.fromHex("05009234"))
    NumberUtil.targetCompression(expanded16) must be(UInt32.fromHex("05009234"))

    val expanded17 = NumberUtil.targetExpansion(UInt32.fromHex("20123456"))
    NumberUtil.targetCompression(expanded17) must be(UInt32.fromHex("20123456"))
    val expanded18 = NumberUtil.targetExpansion(UInt32.fromHex("ff123456"))
    expanded18.isOverflow must be(true)
  }

  private def runTest(
      nBits: UInt32,
      expected: BlockHeader.TargetDifficultyHelper): Assertion = {
    val expansion = NumberUtil.targetExpansion(nBits)
    assert(expansion == expected)
  }
}
