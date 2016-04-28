package org.bitcoins.util

import org.bitcoins.protocol.script.ScriptSignature

import org.bitcoins.protocol.CompactSizeUIntImpl
import org.bitcoins.script.constant.ScriptNumberImpl
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/8/16.
 */
class NumberUtilTest extends FlatSpec with MustMatchers with NumberUtil {


  "NumberUtil" must "convert a positive hex number to its corresponding long number" in {
    val hex = "01"
    val long = toLong(hex)
    long must be (1)


    //127
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

    //20
    val hex5 = "14"
    val long5 = toLong(hex5)
    long5 must be (20)

    //0
    val hex6 = "00"
    val long6 = toLong(hex6)
    long6 must be (0)
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


    //must also work for bytes
    isNegative(BitcoinSUtil.decodeHex(hex2)) must be (true)
  }

  it must "change a sign bit from negative to positive" in {
    val hex = "ff"
    val expectedHex = "7f"
    BitcoinSUtil.encodeHex(changeSignBitToPositive(BitcoinSUtil.decodeHex(hex))) must be (expectedHex)

    //-32767
    val hex1 = "ffff"
    val expectedHex1 = "7fff"
    BitcoinSUtil.encodeHex(changeSignBitToPositive(hex1)) must be (expectedHex1)
  }

  it must "change a sign bit from positive to negative" in {

    val hex = "01"
    val expectedHex = "81"
    BitcoinSUtil.encodeHex(changeSignBitToNegative(hex)) must be (expectedHex)

    //32767
    val hex1 = "7fff"
    val expectedHex1 = "ffff"
    BitcoinSUtil.encodeHex(changeSignBitToNegative(hex1)) must be (expectedHex1)

    //128
    val hex2 = "8000"
    val expectedHex2 = "8000"
    BitcoinSUtil.encodeHex(changeSignBitToNegative(hex2)) must be (expectedHex2)
  }

  it must "detect if the last two bytes are all zeros" in {
    val hex = "00"
    firstByteAllZeros(hex) must be (true)

    val hex1 = "8001"
    firstByteAllZeros(hex1) must be (false)

    val hex2 = "80"
    firstByteAllZeros(hex2) must be (false)
  }



  it must "serialize negative numbers to the correct hex value" in {
    val hex = longToHex(-1)
    val expectedHex = "81"
    hex must be (expectedHex)

    val hex1 = longToHex(-127)
    val expectedHex1 = "ff"
    hex1 must be (expectedHex1)

    val hex2 = longToHex(-128)
    val expectedHex2 = "8080"
    hex2 must be (expectedHex2)

    val hex3 = longToHex(-32767)
    val expectedHex3 = "ffff"
    hex3 must be (expectedHex3)

    val hex4 = longToHex(-32768)
    val expectedHex4 = "008080"
    hex4 must be (expectedHex4)
  }


  it must "serialize a positive number to the correct hex value" in {
    val hex = longToHex(0L)
    val expectedHex = "00"
    hex must be (expectedHex)

    val hex1 = longToHex(1)
    val expectedHex1 = "01"
    hex1 must be (expectedHex1)

    val hex2 = longToHex(127)
    val expectedHex2 = "7f"
    hex2 must be (expectedHex2)

    val hex3 = longToHex(128)
    val expectedHex3 = "8000"
    hex3 must be (expectedHex3)

    val hex4 = longToHex(32767)
    val expectedHex4 = "ff7f"
    hex4 must be (expectedHex4)

    val hex5 = longToHex(32768)
    val expectedHex5 = "008000"
    hex5 must be (expectedHex5)
  }


  it must "parse a variable length integer (VarInt)" in {
    val str = "fdfd00"
    parseCompactSizeUInt(str) must be (CompactSizeUIntImpl(253,3))

    val str1 = "00"
    parseCompactSizeUInt(str1) must be (CompactSizeUIntImpl(0,1))

    val str2 = "ffffffffff"
    parseCompactSizeUInt(str2) must be (CompactSizeUIntImpl(4294967295L,9))
  }


  it must "parse a variable length integer the same from a tx input and a script sig" in {
    parseCompactSizeUInt(TestUtil.txInput.head.scriptSignature) must be (TestUtil.txInput.head.scriptSigCompactSizeUInt)
  }

  it must "parse multiple variable length integers correctly for a multi input tx" in {
    parseCompactSizeUInt(TestUtil.txInputs.head.scriptSignature) must be (TestUtil.txInputs.head.scriptSigCompactSizeUInt)
    parseCompactSizeUInt(TestUtil.txInputs(1).scriptSignature) must be (TestUtil.txInputs(1).scriptSigCompactSizeUInt)
  }

  it must "parse the variable length integer of the empty script" in {
    parseCompactSizeUInt(ScriptSignature.empty) must be (CompactSizeUIntImpl(0,1))
  }

}
