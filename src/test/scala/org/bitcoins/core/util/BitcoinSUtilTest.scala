package org.bitcoins.core.util

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 4/1/16.
 */
class BitcoinSUtilTest extends FlatSpec with MustMatchers {


  "BitcoinSUtil" must "determine if a string is a hex string" in {
    BitcoinSUtil.isHex("abcdef0123456789") must be (true)

    BitcoinSUtil.isHex("") must be (false)

    //don't allow upper case hex chars
    BitcoinSUtil.isHex("ABCDEF0123456789") must be (false)

    BitcoinSUtil.isHex("g") must be (false)

    //fail to parse a hex string that is uneven
    BitcoinSUtil.isHex("123") must be (false)
  }
}
