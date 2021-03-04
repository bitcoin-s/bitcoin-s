package org.bitcoins.core.script.arithmetic

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 1/8/16.
  */
class ArithmeticOperationsFactoryTest extends BitcoinSUnitTest {
  "ArithmeticOperationsFactory" must "match strings with arithmetic operations" in {
    ArithmeticOperation.fromStringOpt("OP_1ADD") must be(Some(OP_1ADD))
    ArithmeticOperation.fromStringOpt("OP_ADD") must be(Some(OP_ADD))
    ArithmeticOperation.fromStringOpt("OP_LESSTHAN") must be(Some(OP_LESSTHAN))
    ArithmeticOperation.fromStringOpt("OP_RANDOM") must be(None)
  }
}
