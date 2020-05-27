package org.bitcoins.core.script.constant

import org.bitcoins.core.util.BytesUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 6/5/16.
  */
class ScriptNumberUtilTest extends BitcoinSUnitTest {
  "ScriptNumberUtil" must "convert a positive hex number to its corresponding long number" in {
    val hex = "01"
    val long = ScriptNumberUtil.toLong(hex)
    long must be(1)

    //127
    val hex1 = "7f"
    val long1 = ScriptNumberUtil.toLong(hex1)
    long1 must be(127)

    //128
    val hex2 = "8000"
    val long2 = ScriptNumberUtil.toLong(hex2)
    long2 must be(128)

    //32767
    val hex3 = "ff7f"
    val long3 = ScriptNumberUtil.toLong(hex3)
    long3 must be(32767)

    //32768
    val hex4 = "008000"
    val long4 = ScriptNumberUtil.toLong(hex4)
    long4 must be(32768)

    //20
    val hex5 = "14"
    val long5 = ScriptNumberUtil.toLong(hex5)
    long5 must be(20)

    //0
    val hex6 = "00"
    val long6 = ScriptNumberUtil.toLong(hex6)
    long6 must be(0)
  }

  it must "convert a negative hex number to its corresponding long number" in {
    //-1
    val hex = "81"
    val long = ScriptNumberUtil.toLong(hex)
    long must be(-1)

    //-127
    val hex1 = "ff"
    val long1 = ScriptNumberUtil.toLong(hex1)
    long1 must be(-127)

    //-128
    val hex2 = "8080"
    val long2 = ScriptNumberUtil.toLong(hex2)
    long2 must be(-128)

    //-32767
    val hex3 = "ffff"
    val long3 = ScriptNumberUtil.toLong(hex3)
    long3 must be(-32767)

    //-32768
    val hex4 = "008080"
    val long4 = ScriptNumberUtil.toLong(hex4)
    long4 must be(-32768)
  }

  it must "determine if a hex string is a positive number" in {
    val hex = "01"
    val hexIsPositive = ScriptNumberUtil.isPositive(hex)
    hexIsPositive must be(true)

    //128
    val hex1 = "8000"
    val hexIsPositive1 = ScriptNumberUtil.isPositive(hex1)
    hexIsPositive1 must be(true)

    val hex2 = "ff7f"
    val hexIsPositive2 = ScriptNumberUtil.isPositive(hex2)
    hexIsPositive2 must be(true)

  }

  it must "determine if a hex string is a negative number" in {
    //-1
    val hex = "81"
    val hexIsNegative = ScriptNumberUtil.isNegative(hex)
    hexIsNegative must be(true)

    //-128
    val hex1 = "8080"
    val hexIsNegative1 = ScriptNumberUtil.isNegative(hex1)
    hexIsNegative1 must be(true)

    //-32767
    val hex2 = "ffff"
    val hexIsNegative2 = ScriptNumberUtil.isNegative(hex2)
    hexIsNegative2 must be(true)

    //must also work for bytes
    ScriptNumberUtil.isNegative(BytesUtil.decodeHex(hex2)) must be(true)
  }

  it must "change a sign bit from negative to positive" in {
    val hex = "ff"
    val expectedHex = "7f"
    BytesUtil.encodeHex(
      ScriptNumberUtil
        .changeSignBitToPositive(BytesUtil.decodeHex(hex))) must be(expectedHex)

    //-32767
    val hex1 = "ffff"
    val expectedHex1 = "7fff"
    BytesUtil.encodeHex(ScriptNumberUtil.changeSignBitToPositive(hex1)) must be(
      expectedHex1)
  }

  it must "change a sign bit from positive to negative" in {

    val hex = "01"
    val expectedHex = "81"
    BytesUtil.encodeHex(ScriptNumberUtil.changeSignBitToNegative(hex)) must be(
      expectedHex)

    //32767
    val hex1 = "7fff"
    val expectedHex1 = "ffff"
    BytesUtil.encodeHex(ScriptNumberUtil.changeSignBitToNegative(hex1)) must be(
      expectedHex1)

    //128
    val hex2 = "8000"
    val expectedHex2 = "8000"
    BytesUtil.encodeHex(ScriptNumberUtil.changeSignBitToNegative(hex2)) must be(
      expectedHex2)
  }

  it must "detect if the last two bytes are all zeros" in {
    val hex = "00"
    ScriptNumberUtil.firstByteAllZeros(hex) must be(true)

    val hex1 = "8001"
    ScriptNumberUtil.firstByteAllZeros(hex1) must be(false)

    val hex2 = "80"
    ScriptNumberUtil.firstByteAllZeros(hex2) must be(false)
  }

  it must "serialize negative numbers to the correct hex value" in {
    val hex = ScriptNumberUtil.longToHex(-1)
    val expectedHex = "81"
    hex must be(expectedHex)

    val hex1 = ScriptNumberUtil.longToHex(-127)
    val expectedHex1 = "ff"
    hex1 must be(expectedHex1)

    val hex2 = ScriptNumberUtil.longToHex(-128)
    val expectedHex2 = "8080"
    hex2 must be(expectedHex2)

    val hex3 = ScriptNumberUtil.longToHex(-32767)
    val expectedHex3 = "ffff"
    hex3 must be(expectedHex3)

    val hex4 = ScriptNumberUtil.longToHex(-32768)
    val expectedHex4 = "008080"
    hex4 must be(expectedHex4)
  }

  it must "serialize a positive number to the correct hex value" in {
    val hex = ScriptNumberUtil.longToHex(0L)
    val expectedHex = "00"
    hex must be(expectedHex)

    val hex1 = ScriptNumberUtil.longToHex(1)
    val expectedHex1 = "01"
    hex1 must be(expectedHex1)

    val hex2 = ScriptNumberUtil.longToHex(127)
    val expectedHex2 = "7f"
    hex2 must be(expectedHex2)

    val hex3 = ScriptNumberUtil.longToHex(128)
    val expectedHex3 = "8000"
    hex3 must be(expectedHex3)

    val hex4 = ScriptNumberUtil.longToHex(32767)
    val expectedHex4 = "ff7f"
    hex4 must be(expectedHex4)

    val hex5 = ScriptNumberUtil.longToHex(32768)
    val expectedHex5 = "008000"
    hex5 must be(expectedHex5)
  }

  it must "convert a sequence of bytes to a max value for int" in {
    val max = Int.MaxValue
    ScriptNumberUtil.toInt("FFFFFF7F") must be(max)
  }

  it must "convert a sequence of bytes to  the min value for an int" in {
    val min = Int.MinValue + 1
    //the minimum number we can represent in ScriptNumbers i
    //is Int.MinValue + 1 since we have a negative zero and zero
    ScriptNumberUtil.toInt("ffffffff") must be(min)
  }

  it must "throw an exception when we try and convert a 33 bit sequence to an int" in {
    intercept[IllegalArgumentException] {
      ScriptNumberUtil.toInt("FFFFFF7F00")
    }
  }

}
