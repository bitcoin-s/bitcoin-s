package org.bitcoins.core.currency


import org.bitcoins.core.number.{Int32, Int64}
import org.scalatest.{FlatSpec, Matchers, MustMatchers}

/**
 * Created by chris on 12/21/15.
 */
class CurrencyUnitsTest extends FlatSpec with MustMatchers {
  "CurrencyUnits" must "have the correct amount for the max amount of satoshis" in {
    Satoshis.max.underlying must be (Int64.max)
  }

  it must "have the correct number for zero units of currency" in {
    CurrencyUnits.zero.underlying must be (Int64.zero)
  }

}