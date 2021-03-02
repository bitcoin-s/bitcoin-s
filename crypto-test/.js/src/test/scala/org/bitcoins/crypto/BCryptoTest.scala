package org.bitcoins.crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class BCryptoTest extends AnyFlatSpec with Matchers {
  behavior of "BCrypto"

  it must "work" in {
    println("test")
    assert("1" != "2")
  }
}
