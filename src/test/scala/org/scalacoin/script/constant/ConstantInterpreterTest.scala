package org.scalacoin.script.constant

import org.scalacoin.script.ScriptProgramImpl
import org.scalacoin.script.bitwise.OP_EQUAL
import org.scalacoin.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/24/16.
 */
class ConstantInterpreterTest extends FlatSpec with MustMatchers with ConstantInterpreter {

  "ConstantInterpreter" must "interpret OP_PUSHDATA1 correctly" in {
    val stack = List()
    val script = List(OP_PUSHDATA1, ScriptNumberImpl(1), ScriptNumberImpl(7), OP_7, OP_EQUAL)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction)
    val newProgram = opPushData1(program)

    newProgram.stack must be (List(ScriptNumberImpl(7)))
    newProgram.script must be (List(OP_7,OP_EQUAL))
  }

  it must "interpret OP_PUSHDATA2 correctly" in {
    val stack = List()
    val script = List(OP_PUSHDATA2, ScriptConstantImpl("0100"), ScriptNumberImpl(8), OP_8, OP_EQUAL)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction)
    val newProgram = opPushData2(program)
    newProgram.stack must be (List(ScriptNumberImpl(8)))
    newProgram.script must be (List(OP_8,OP_EQUAL))
  }

  it must "interpret OP_PUSHDATA4 correctly" in {
    val stack = List()
    val script = List(OP_PUSHDATA4, ScriptConstantImpl("01000000"), ScriptNumberImpl(9), OP_9, OP_EQUAL)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction)
    val newProgram = opPushData4(program)
    newProgram.stack must be (List(ScriptNumberImpl(9)))
    newProgram.script must be (List(OP_9, OP_EQUAL))
  }


  it must "push a constant 2 bytes onto the stack" in {
    val stack = List()
    val script = List(ScriptNumberImpl(2), ScriptNumberImpl(1), OP_0)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction)
    val newProgram = pushScriptNumberBytesToStack(program)
    newProgram.script.isEmpty must be (true)
    newProgram.stack must be (List(ScriptConstantImpl("0100")))
  }
}
