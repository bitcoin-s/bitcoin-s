package org.bitcoins.core.util

import org.scalatest.{ FlatSpec, MustMatchers }

/**
 * Created by chris on 4/1/16.
 */
class BitcoinSUtilTest extends FlatSpec with MustMatchers {

  "BitcoinSUtil" must "determine if a string is a hex string" in {
    BitcoinSUtil.isHex("abcdef0123456789") must be(true)

    BitcoinSUtil.isHex("") must be(false)

    //don't allow upper case hex chars
    BitcoinSUtil.isHex("ABCDEF0123456789") must be(false)

    BitcoinSUtil.isHex("g") must be(false)

    //fail to parse a hex string that is uneven
    BitcoinSUtil.isHex("123") must be(false)
  }

  it must "convert a byte to a bit vector" in {
    val byte = 0.toByte
    BitcoinSUtil.byteToBitVector(byte) must be(Seq(false, false, false, false, false, false, false, false))

    val byte1 = 1.toByte
    BitcoinSUtil.byteToBitVector(byte1) must be(Seq(false, false, false, false, false, false, false, true))

    val byte2 = 2.toByte
    BitcoinSUtil.byteToBitVector(byte2) must be(Seq(false, false, false, false, false, false, true, false))

    val byte3 = 3.toByte
    BitcoinSUtil.byteToBitVector(byte3) must be(Seq(false, false, false, false, false, false, true, true))

    val maxByte = 0xff.toByte
    BitcoinSUtil.byteToBitVector(maxByte) must be(Seq(true, true, true, true, true, true, true, true))
  }

  it must "convert a bit vector to a byte" in {
    val bitVector0 = Seq(false, false, false, false, false, false, false, false)
    BitcoinSUtil.bitVectorToByte(bitVector0) must be(0.toByte)

    val bitVector1 = Seq(false, false, false, false, false, false, false, true)
    BitcoinSUtil.bitVectorToByte(bitVector1) must be(1.toByte)

    val bitVector2 = Seq(false, false, false, false, false, false, true, false)
    BitcoinSUtil.bitVectorToByte(bitVector2) must be(2.toByte)

    val bitVectorMax = Seq(true, true, true, true, true, true, true, true)
    BitcoinSUtil.bitVectorToByte(bitVectorMax) must be(0xff.toByte)

  }
}
