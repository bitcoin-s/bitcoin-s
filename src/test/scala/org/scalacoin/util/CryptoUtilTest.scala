package org.scalacoin.util

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/26/16.
 */
class CryptoUtilTest extends FlatSpec with MustMatchers {


  "CryptoUtil" must "perform a SHA-1 hash" in {
    val hash = CryptoUtil.sha1("")
    hash must be ("da39a3ee5e6b4b0d3255bfef95601890afd80709")
  }

  it must "perform the correct SHA-1 hash on a mneomnic seed" in {
    val str = "The quick brown fox jumps over the lazy dog"
    CryptoUtil.sha1(str) must be ("2fd4e1c67a2d28fced849ee1bb76e7391b93eb12")
  }
}
