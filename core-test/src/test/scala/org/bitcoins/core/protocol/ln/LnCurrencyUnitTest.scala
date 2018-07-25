package org.bitcoins.core.protocol.ln

import org.scalatest.{ FlatSpec, MustMatchers }

class LnCurrencyUnitTest extends FlatSpec with MustMatchers {
  it must "serialize MicroBitcoins to string" in {
    val microBitcoins = MicroBitcoins(1000)
    microBitcoins.toEncodedString must be("1000u")
  }
}