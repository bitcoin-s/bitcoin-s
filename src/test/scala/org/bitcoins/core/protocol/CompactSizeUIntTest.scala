package org.bitcoins.core.protocol

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.util.{ BitcoinSUtil, TestUtil }
import org.scalatest.{ FlatSpec, MustMatchers }

/**
 * Created by chris on 7/26/15.
 */
class CompactSizeUIntTest extends FlatSpec with MustMatchers {

  "CompactSizeUInt" must "serialize a VarInt with size 1 correctly" in {
    val varInt = CompactSizeUInt(UInt64(139), 1)
    varInt.hex must be("8b")
  }

  it must "serialize a VarInt with that is the number zero correctly" in {
    val varInt = CompactSizeUInt(UInt64.zero, 1)
    varInt.hex must be("00")
  }

  it must "serialize a compact size uint representing 255" in {
    val compactSizeUInt = CompactSizeUInt(UInt64(255))
    compactSizeUInt must be(CompactSizeUInt(UInt64(255), 3))
    compactSizeUInt.hex must be("fdff00")
  }

  it must "calculate the varint for the following hex string" in {
    CompactSizeUInt.calculateCompactSizeUInt("00") must be(CompactSizeUInt(UInt64.one, 1))

    //for a string that is 256 bytes long
    val byteSeq256Size = for (_ <- 0 until 256) yield 0.toByte
    CompactSizeUInt.calculateCompactSizeUInt(byteSeq256Size) must be(CompactSizeUInt(UInt64(256), 3))
  }

  it must "calculate the correct compact size uint for a number 515 bytes long" in {
    //from the bitcoin developer reference
    //https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers
    val byteSeq515Size = for (_ <- 0 until 515) yield 0.toByte
    val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(byteSeq515Size)
    compactSizeUInt must be(CompactSizeUInt(UInt64(515), 3))
    compactSizeUInt.hex must be("fd0302")
  }

  it must "calculate correct compact size uint for a number 500,000 bytes long" in {
    val byteSeq500000Size = for (_ <- 0 until 500000) yield 0.toByte
    val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(byteSeq500000Size)
    compactSizeUInt must be(CompactSizeUInt(UInt64(500000), 5))
    compactSizeUInt.hex must be("fe20a10700")
  }

  it must "parse a compact size uint from bytes" in {
    val str = "fd0302"
    val bytes = BitcoinSUtil.decodeHex(str)
    CompactSizeUInt.fromBytes(bytes) must be(CompactSizeUInt(UInt64(515), 3))
  }

  it must "parse a variable length integer (VarInt)" in {
    val str = "fdfd00"
    CompactSizeUInt.parseCompactSizeUInt(str) must be(CompactSizeUInt(UInt64(253), 3))

    val str1 = "00"
    CompactSizeUInt.parseCompactSizeUInt(str1) must be(CompactSizeUInt(UInt64.zero, 1))

    val str2 = "fe20a10700"
    CompactSizeUInt.parseCompactSizeUInt(str2) must be(CompactSizeUInt(UInt64(500000)))

    val str3 = "ffffffffff"
    CompactSizeUInt.parseCompactSizeUInt(str3) must be(CompactSizeUInt(UInt64(4294967295L), 9))
  }

  it must "parse a variable length integer the same from a tx input and a script sig" in {
    CompactSizeUInt.parseCompactSizeUInt(TestUtil.txInput.scriptSignature.bytes) must be(TestUtil.txInput.scriptSignature.compactSizeUInt)
  }

  it must "parse the variable length integer of the empty script" in {
    CompactSizeUInt.parseCompactSizeUInt(ScriptSignature.empty) must be(CompactSizeUInt(UInt64.one, 1))
  }

  it must "parse variable length integer of script sig at least 0xffff bytes in length, and greater than 0xffffffff" in {
    val c = CompactSizeUInt.calculateCompactSizeUInt(_: String)
    val s1NoCmpct = TestUtil.rawP2shInputScriptLargeSignature * 50
    val s2NoCmpct = TestUtil.rawP2shInputScriptLargeSignature * 120
    val s1 = c(s1NoCmpct).hex + s1NoCmpct
    val s2 = c(s2NoCmpct).hex + s2NoCmpct
    CompactSizeUInt.parseCompactSizeUInt(ScriptSignature(s1)) must be(CompactSizeUInt(UInt64(30453), 3))
    CompactSizeUInt.parseCompactSizeUInt(ScriptSignature(s2)) must be(CompactSizeUInt(UInt64(73085), 5))
  }

  it must "parse 8 bit, 16 bit, 32 bit number and 64 bit number as compactsizeuints" in {
    val bit8 = 1.toByte
    val bit16 = 253.toByte
    val bit32 = 254.toByte
    val bit64 = 255.toByte
    CompactSizeUInt.parseCompactSizeUIntSize(bit8) must be(1)
    CompactSizeUInt.parseCompactSizeUIntSize(bit16) must be(3)
    CompactSizeUInt.parseCompactSizeUIntSize(bit32) must be(5)
    CompactSizeUInt.parseCompactSizeUIntSize(bit64) must be(9)
  }

  it must "intercept a failed requirement when the byte array size is zero" in {
    intercept[IllegalArgumentException] {
      val emptyBytes: Seq[Byte] = Seq()
      CompactSizeUInt.parseCompactSizeUInt(emptyBytes)
    }
  }
}
