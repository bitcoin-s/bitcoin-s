package org.bitcoins.core.script.arithmetic

import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 1/8/16.
  */
class ArithmeticOperationsFactoryTest extends BitcoinSUnitTest {
  "ArithmeticOperationsFactory" must "match strings with arithmetic operations" in {
    ArithmeticOperation.fromString("OP_1ADD") must be(Some(OP_1ADD))
    ArithmeticOperation.fromString("OP_ADD") must be(Some(OP_ADD))
    ArithmeticOperation.fromString("OP_LESSTHAN") must be(Some(OP_LESSTHAN))
    ArithmeticOperation.fromString("OP_RANDOM") must be(None)
  }
}
