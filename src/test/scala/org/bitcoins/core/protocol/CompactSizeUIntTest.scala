package org.bitcoins.core.protocol

import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 7/26/15.
 */
class CompactSizeUIntTest extends FlatSpec with MustMatchers  {



  "CompactSizeUInt" must "serialize a VarInt with size 1 correctly" in {
    val varInt = CompactSizeUInt(139,1)
    varInt.hex must be ("8b")
  }

  it must "serialize a VarInt with that is the number zero correctly" in {
    val varInt = CompactSizeUInt(0,1)
    varInt.hex must be ("00")
  }

  it must "calculate the varint for the following hex string" in {
    CompactSizeUInt.calculateCompactSizeUInt("00") must be (CompactSizeUInt(1,1))

    //for a string that is 256 bytes long
    val byteSeq256Size = for (_ <- 0 until 256) yield 0.toByte
    CompactSizeUInt.calculateCompactSizeUInt(byteSeq256Size) must be (CompactSizeUInt(256,3))
  }

  it must "calculate the correct compact size uint for a number 515 bytes long" in {
    //from the bitcoin developer reference
    //https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers
    val byteSeq515Size = for (_ <- 0 until 515) yield 0.toByte
    val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(byteSeq515Size)
    compactSizeUInt must be (CompactSizeUInt(515,3))
    compactSizeUInt.hex must be ("fd0302")
  }
}
