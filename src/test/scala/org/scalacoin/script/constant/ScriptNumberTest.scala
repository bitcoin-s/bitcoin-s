package org.scalacoin.script.constant

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/25/16.
 */
class ScriptNumberTest extends FlatSpec with MustMatchers {

  val zero = ScriptNumberFactory.zero
  val one = ScriptNumberFactory.one
  val ten = ScriptNumberFactory.fromNumber(10)
  "ScriptNumber" must "derive the correct hex value from a script number" in {
    ScriptNumberImpl(1).hex must be ("01")

    ScriptNumberImpl(8).hex must be ("08")
  }

  it must "add two script numbers correctly" in {
    (zero + zero) must be (zero)
    (one + zero) must be (one)
    (one + ten) must be (ScriptNumberImpl(11))
  }

  it must "subtract two script numbers correctly" in {
    (zero - zero) must equal (zero)
    (one - zero) must equal (one)
    (ten - one) must equal (ScriptNumberImpl(9))
  }

  it must "multiply two script numbers correctly" in {
    (zero * zero) must equal (zero)
    (one * zero) must equal (zero)
    (ten * one) must equal (ten)
    (ten * ScriptNumberImpl(5)) must equal (ScriptNumberImpl(50))
  }

}
