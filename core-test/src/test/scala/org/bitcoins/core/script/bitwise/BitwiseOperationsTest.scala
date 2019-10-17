package org.bitcoins.core.script.bitwise

import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 1/6/16.
  */
class BitwiseOperationsTest extends BitcoinSUnitTest {

  "BitwiseOperations" must "define OP_EQUAL" in {
    OP_EQUAL.opCode must be(135)
  }

  it must "define OP_EQUALVERIFY" in {
    OP_EQUALVERIFY.opCode must be(136)
  }
}
