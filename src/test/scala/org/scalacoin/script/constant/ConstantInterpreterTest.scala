package org.scalacoin.script.constant

import org.scalacoin.script.bitwise.OP_EQUAL
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/24/16.
 */
class ConstantInterpreterTest extends FlatSpec with MustMatchers with ConstantInterpreter {

  "ConstantInterpreter" must "interpret OP_PUSHDATA1 correctly" in {
    val stack = List()
    val script = List(OP_PUSHDATA1, ScriptNumberImpl(1), ScriptNumberImpl(7), OP_7, OP_EQUAL)
    val (newStack,newScript) = opPushData1(stack,script)

    newStack must be (List(ScriptNumberImpl(7)))
    newScript must be (List(OP_7,OP_EQUAL))
  }

  it must "interpret OP_PUSHDATA2 correctly" in {
    val stack = List()
    val script = List(OP_PUSHDATA2, ScriptConstantImpl("0100"), ScriptNumberImpl(8), OP_8, OP_EQUAL)
    val (newStack,newScript) = opPushData2(stack,script)
    newStack must be (List(ScriptNumberImpl(8)))
    newScript must be (List(OP_8,OP_EQUAL))
  }

  it must "interpret OP_PUSHDATA4 correctly" in {
    val stack = List()
    val script = List(OP_PUSHDATA4, ScriptConstantImpl("01000000"), ScriptNumberImpl(9), OP_9, OP_EQUAL)
    val (newStack,newScript) = opPushData4(stack,script)
    newStack must be (List(ScriptNumberImpl(9)))
    newScript must be (List(OP_9, OP_EQUAL))
  }
}
