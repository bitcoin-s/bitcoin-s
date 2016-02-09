package org.scalacoin.util

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/8/16.
 */
class NumberUtilTest extends FlatSpec with MustMatchers with NumberUtil {


  "NumberUtil" must "convert a positive hex number to its corresponding long number" in {
    val hex = "01"
    val long = toLong(hex)
    long must be (1)

    val hex1 = "7f"
    val long1 = toLong(hex1)
    long1 must be (127)

    //128
    val hex2 = "8000"
    val long2 = toLong(hex2)
    long2 must be (128)

    //32767
    val hex3 = "ff7f"
    val long3 = toLong(hex3)
    long3 must be (32767)

    //32768
    val hex4 = "008000"
    val long4 = toLong(hex4)
    long4 must be (32768)

  }

  it must "convert a negative hex number to its corresponding long number" in  {
    //-1
    val hex = "81"
    val long = toLong(hex)
    long must be (-1)
    //-127
    val hex1 = "ff"
    val long1 = toLong(hex1)
    long1 must be (-127)

    //-128
    val hex2 = "8080"
    val long2 = toLong(hex2)
    long2 must be (-128)

    //-32767
    val hex3 = "ffff"
    val long3 = toLong(hex3)
    long3 must be (-32767)

    //-32768
    val hex4 = "008080"
    val long4 = toLong(hex4)
    long4 must be (-32768)
  }

  it must "determine if a hex string is a positive number" in  {
    val hex = "01"
    val hexIsPositive = isPositive(hex)
    hexIsPositive must be (true)

    //128
    val hex1 = "8000"
    val hexIsPositive1 = isPositive(hex1)
    hexIsPositive1 must be (true)

    val hex2 = "ff7f"
    val hexIsPositive2 = isPositive(hex2)
    hexIsPositive2 must be (true)
  }

  it must "determine if a hex string is a negative number" in {
    //-1
    val hex = "81"
    val hexIsNegative = isNegative(hex)
    hexIsNegative must be (true)

    //-128
    val hex1 = "8080"
    val hexIsNegative1 = isNegative(hex1)
    hexIsNegative1 must be (true)

    //-32767
    val hex2 = "ffff"
    val hexIsNegative2 = isNegative(hex2)
    hexIsNegative2 must be (true)
  }

  it must "change a sign bit from negative to positive" in {
    val hex = "ff"
    val expectedHex = "7f"
    ScalacoinUtil.encodeHex(changeSignBitToPositive(ScalacoinUtil.decodeHex(hex))) must be (expectedHex)

    //-32767
    val hex1 = "ffff"
    val expectedHex1 = "7fff"
    ScalacoinUtil.encodeHex(changeSignBitToPositive(ScalacoinUtil.decodeHex(hex1))) must be (expectedHex1)
  }

  it must "detect if the last two bytes are all zeros" in {
    val hex = "00"
    firstByteAllZeros(hex) must be (true)

    val hex1 = "8001"
    firstByteAllZeros(hex1) must be (false)

    val hex2 = "80"
    firstByteAllZeros(hex2) must be (false)
  }
}
