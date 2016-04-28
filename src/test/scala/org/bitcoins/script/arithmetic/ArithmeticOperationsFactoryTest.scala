package org.bitcoins.script.arithmetic

import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/8/16.
 */
class ArithmeticOperationsFactoryTest extends FlatSpec with MustMatchers {
  "ArithmeticOperationsFactory" must "match strings with arithmetic operations" in {
    ArithmeticOperation.fromString("OP_1ADD") must be (Some(OP_1ADD))
    ArithmeticOperation.fromString("OP_ADD") must be (Some(OP_ADD))
    ArithmeticOperation.fromString("OP_LESSTHAN") must be (Some(OP_LESSTHAN))
    ArithmeticOperation.fromString("OP_RANDOM") must be (None)
  }
}
