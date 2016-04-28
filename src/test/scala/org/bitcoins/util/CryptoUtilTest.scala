package org.bitcoins.util

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/26/16.
 */
class CryptoUtilTest extends FlatSpec with MustMatchers {


  "CryptoUtil" must "perform a SHA-1 hash" in {
    val hash = CryptoUtil.sha1("")
    BitcoinSUtil.encodeHex(hash) must be ("da39a3ee5e6b4b0d3255bfef95601890afd80709")
  }


  it must "perform the correct RIPEMD160 on a string" in {
    val str = ""
    val expectedDigest = "9c1185a5c5e9fc54612808977ee8f548b2258d31"
    BitcoinSUtil.encodeHex(CryptoUtil.ripeMd160(str)) must be (expectedDigest)
  }

  it must "perform a RIPEMD160 on a SHA256 hash to generate a bitcoin address" in {
    //https://bitcoin.stackexchange.com/questions/37040/ripemd160sha256publickey-where-am-i-going-wrong
    val str = "ea571f53cb3a9865d3dc74735e0c16643d319c6ad81e199b9c8408cecbcec7bb"
    val expected = "5238c71458e464d9ff90299abca4a1d7b9cb76ab"
    BitcoinSUtil.encodeHex(CryptoUtil.ripeMd160(str)) must be (expected)
  }

  it must "perform a single SHA256 hash" in {
    val hex = ""
    val strBytes = BitcoinSUtil.decodeHex(hex)
    val expected = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    BitcoinSUtil.encodeHex(CryptoUtil.sha256(strBytes)) must be (expected)
    BitcoinSUtil.encodeHex(CryptoUtil.sha256(hex)) must be (expected)



  }
}
