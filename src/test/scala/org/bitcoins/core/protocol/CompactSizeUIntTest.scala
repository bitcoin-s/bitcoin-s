package org.bitcoins.core.protocol

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 7/26/15.
 */
class CompactSizeUIntTest extends FlatSpec with MustMatchers  {

  "CompactSizeUInt" must "serialize a VarInt with size 1 correctly" in {
    val varInt = CompactSizeUInt(UInt64(139),1)
    varInt.hex must be ("8b")
  }

  it must "serialize a VarInt with that is the number zero correctly" in {
    val varInt = CompactSizeUInt(UInt64.zero,1)
    varInt.hex must be ("00")
  }

  it must "serialize a compact size uint representing 255" in {
    val compactSizeUInt = CompactSizeUInt(UInt64(255))
    compactSizeUInt must be (CompactSizeUInt(UInt64(255),3))
    compactSizeUInt.hex must be ("fdff00")
  }

  it must "calculate the varint for the following hex string" in {
    CompactSizeUInt.calculateCompactSizeUInt("00") must be (CompactSizeUInt(UInt64.one,1))

    //for a string that is 256 bytes long
    val byteSeq256Size = for (_ <- 0 until 256) yield 0.toByte
    CompactSizeUInt.calculateCompactSizeUInt(byteSeq256Size) must be (CompactSizeUInt(UInt64(256),3))
  }

  it must "calculate the correct compact size uint for a number 515 bytes long" in {
    //from the bitcoin developer reference
    //https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers
    val byteSeq515Size = for (_ <- 0 until 515) yield 0.toByte
    val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(byteSeq515Size)
    compactSizeUInt must be (CompactSizeUInt(UInt64(515),3))
    compactSizeUInt.hex must be ("fd0302")
  }



  it must "parse a variable length integer (VarInt)" in {
    val str = "fdfd00"
    CompactSizeUInt.parseCompactSizeUInt(str) must be (CompactSizeUInt(UInt64(253),3))

    val str1 = "00"
    CompactSizeUInt.parseCompactSizeUInt(str1) must be (CompactSizeUInt(UInt64.zero,1))

    val str2 = "ffffffffff"
    CompactSizeUInt.parseCompactSizeUInt(str2) must be (CompactSizeUInt(UInt64(4294967295L),9))
  }


  it must "parse a variable length integer the same from a tx input and a script sig" in {
    CompactSizeUInt.parseCompactSizeUInt(TestUtil.txInput.head.scriptSignature) must be (TestUtil.txInput.head.scriptSigCompactSizeUInt)
  }

  it must "parse multiple variable length integers correctly for a multi input tx" in {
    CompactSizeUInt.parseCompactSizeUInt(TestUtil.txInputs.head.scriptSignature) must be (TestUtil.txInputs.head.scriptSigCompactSizeUInt)
    CompactSizeUInt.parseCompactSizeUInt(TestUtil.txInputs(1).scriptSignature) must be (TestUtil.txInputs(1).scriptSigCompactSizeUInt)
  }

  it must "parse the variable length integer of the empty script" in {
    CompactSizeUInt.parseCompactSizeUInt(ScriptSignature.empty) must be (CompactSizeUInt(UInt64.zero,1))
  }

}
