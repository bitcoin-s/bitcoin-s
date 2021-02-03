package org.bitcoins.core.script.constant

import org.bitcoins.core.number.Int64
import org.bitcoins.testkit.util.BitcoinSUnitTest

/** Created by chris on 1/25/16.
  */
class ScriptNumberTest extends BitcoinSUnitTest {

  val zero = ScriptNumber.zero
  val one = ScriptNumber.one
  val ten = ScriptNumber(10)
  "ScriptNumber" must "derive the correct hex value from a script number" in {
    ScriptNumber(1).hex must be("01")

    ScriptNumber(8).hex must be("08")
  }

  it must "add two script numbers correctly" in {
    (zero + zero) must be(zero)
    (one + zero) must be(one)
    (one + ten) must be(ScriptNumber(11))
  }

  it must "subtract two script numbers correctly" in {
    (zero - zero) must equal(zero)
    (one - zero) must equal(one)
    (ten - one) must equal(ScriptNumber(9))
  }

  it must "multiply two script numbers correctly" in {
    (zero * zero) must equal(zero)
    (one * zero) must equal(zero)
    (ten * one) must equal(ten)
    (ten * ScriptNumber(5)) must equal(ScriptNumber(50))
  }

  it must "compare ScriptNumbers to Int64 correctly" in {
    (zero < Int64.one) must equal(true)
    (zero <= Int64.zero) must equal(true)
    (one > Int64.zero) must equal(true)
    (one >= Int64.one) must equal(true)
  }

  it must "compute bitwise operations correctly" in {
    (ScriptNumber.one & Int64.one).toInt must be(1)
    (ScriptNumber.one & ScriptNumber.one).toInt must be(1)
    (ScriptNumber.one | ScriptNumber.one).toInt must be(1)
  }
}
