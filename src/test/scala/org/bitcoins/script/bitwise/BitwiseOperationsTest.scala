package org.bitcoins.script.bitwise

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/6/16.
 */
class BitwiseOperationsTest extends FlatSpec with MustMatchers {


  "BitwiseOperations" must "define OP_EQUAL" in {
    OP_EQUAL.opCode must be (135)
  }

  it must "define OP_EQUALVERIFY" in {
    OP_EQUALVERIFY.opCode must be (136)
  }
}
