package org.scalacoin.script.bitwise

import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/6/16.
 */
class BitwiseInterpreterTest extends FlatSpec with MustMatchers with BitwiseInterpreter {

  "BitwiseInterpreter" must "evaluate OP_EQUAL" in {
    val pubKeyHash = "5238C71458E464D9FF90299ABCA4A1D7B9CB76AB".toLowerCase
    val stack = List(pubKeyHash, pubKeyHash)
    val script = List(OP_EQUAL)

    val (newStack,newScript) = equal(stack,script)

    newStack.head.toInt must be (1)
  }

}
