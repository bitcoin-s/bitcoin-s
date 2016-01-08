package org.scalacoin.script.arithmetic

import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/8/16.
 */
class ArithmeticOperationsFactoryTest extends FlatSpec with MustMatchers with ArithmeticOperationsFactory {
  "ArithmeticOperationsFactory" must "match strings with arithmetic operations" in {
    fromString("OP_1ADD") must be (Some(OP_1ADD))
    fromString("OP_ADD") must be (Some(OP_ADD))
    fromString("OP_LESSTHAN") must be (Some(OP_LESSTHAN))
    fromString("OP_RANDOM") must be (None)
  }
}
