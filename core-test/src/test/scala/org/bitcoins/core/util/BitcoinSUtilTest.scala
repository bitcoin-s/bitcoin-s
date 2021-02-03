package org.bitcoins.core.util

import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.BitVector

/** Created by chris on 4/1/16.
  */
class BitcoinSUtilTest extends BitcoinSUnitTest {

  "BitcoinSUtil" must "determine if a string is a hex string" in {
    BytesUtil.isHex("abcdef0123456789") must be(true)

    BytesUtil.isHex("") must be(false)

    //don't allow upper case hex chars
    BytesUtil.isHex("ABCDEF0123456789") must be(false)

    BytesUtil.isHex("g") must be(false)

    //fail to parse a hex string that is uneven
    BytesUtil.isHex("123") must be(false)
  }

  it must "convert a byte to a bit vector" in {
    val byte = 0.toByte
    BytesUtil.byteToBitVector(byte).toIndexedSeq must be(
      Seq(false, false, false, false, false, false, false, false))

    val byte1 = 1.toByte
    BytesUtil.byteToBitVector(byte1).toIndexedSeq must be(
      Seq(false, false, false, false, false, false, false, true))

    val byte2 = 2.toByte
    BytesUtil.byteToBitVector(byte2).toIndexedSeq must be(
      Seq(false, false, false, false, false, false, true, false))

    val byte3 = 3.toByte
    BytesUtil.byteToBitVector(byte3).toIndexedSeq must be(
      Seq(false, false, false, false, false, false, true, true))

    val maxByte = 0xff.toByte
    BytesUtil.byteToBitVector(maxByte).toIndexedSeq must be(
      Seq(true, true, true, true, true, true, true, true))
  }

  it must "convert a bit vector to a byte" in {
    val bitVector0 = BitVector.bits(
      Seq(false, false, false, false, false, false, false, false))
    BytesUtil.bitVectorToBytes(bitVector0).toByte() must be(0.toByte)

    val bitVector1 =
      BitVector.bits(Seq(false, false, false, false, false, false, false, true))
    BytesUtil.bitVectorToBytes(bitVector1).toByte() must be(1.toByte)

    val bitVector2 =
      BitVector.bits(Seq(false, false, false, false, false, false, true, false))
    BytesUtil.bitVectorToBytes(bitVector2).toByte() must be(2.toByte)

    val bitVectorMax =
      BitVector.bits(Seq(true, true, true, true, true, true, true, true))
    BytesUtil.bitVectorToBytes(bitVectorMax).toByte() must be(0xff.toByte)

  }
}
