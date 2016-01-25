package org.scalacoin.util

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/21/16.
 */
class ScalacoinUtilTest extends FlatSpec with MustMatchers with ScalacoinUtil {

  "ScalacoinUtil" must "identify if a string is a hex string" in {


    isHex("") must be (true)
    isHex("    ") must be (true)
    isHex("abcdef") must be (true)
    isHex("0123456789abcdef") must be (true)

    isHex("z") must be (false)
    isHex("0123456789abcdefg") must be (false)
  }


  it must "convert a hex string from little endian to big endian" in  {
    littleEndianToBigEndian("01") must be ("01")
    littleEndianToBigEndian("0100") must be ("0001")
    littleEndianToBigEndian("abcd") must be ("cdab")

  }


}
