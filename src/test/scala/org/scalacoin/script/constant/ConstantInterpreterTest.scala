package org.scalacoin.script.constant

import org.scalacoin.script.{ScriptProgramFactory, ScriptProgramImpl}
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
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opPushData1(program)

    newProgram.script must be (List(ScriptNumberImpl(7), OP_7,OP_EQUAL))
  }

  it must "interpret OP_PUSHDATA2 correctly" in {
    val stack = List()
    val script = List(OP_PUSHDATA2, ScriptConstantImpl("0100"), ScriptNumberImpl(8), OP_8, OP_EQUAL)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opPushData2(program)
    newProgram.stack must be (List())
    newProgram.script must be (List(ScriptNumberImpl(8), OP_8,OP_EQUAL))
  }

  it must "interpret OP_PUSHDATA4 correctly" in {
    val stack = List()
    val script = List(OP_PUSHDATA4, ScriptConstantImpl("01000000"), ScriptNumberImpl(9), OP_9, OP_EQUAL)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opPushData4(program)
    newProgram.stack must be (List())
    newProgram.script must be (List(ScriptNumberImpl(9),OP_9, OP_EQUAL))
  }


  it must "push a constant 2 bytes onto the stack" in {
    val stack = List()
    val script = List(BytesToPushOntoStackImpl(2), ScriptNumberFactory.one, OP_0)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = pushScriptNumberBytesToStack(program)
    newProgram.script.isEmpty must be (true)
    newProgram.stack must be (List(ScriptConstantFactory.fromHex("01")))
  }

  it must "push 0 bytes onto the stack which is OP_0" in {
    val stack = List()
    val script = List(OP_PUSHDATA1,BytesToPushOntoStackImpl(0))
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram  = opPushData1(program)
    newProgram.stack must be (List(OP_0))

    val stack1 = List()
    val script1 = List(OP_PUSHDATA2,BytesToPushOntoStackImpl(0))
    val program1 = ScriptProgramFactory.factory(TestUtil.testProgram, stack1,script1)
    val newProgram1  = opPushData2(program1)
    newProgram1.stack must be (List(OP_0))

    val stack2 = List()
    val script2 = List(OP_PUSHDATA4,BytesToPushOntoStackImpl(0))
    val program2 = ScriptProgramFactory.factory(TestUtil.testProgram, stack2,script2)
    val newProgram2 = opPushData4(program2)
    newProgram2.stack must be (List(OP_0))
  }

  it must "throw an illegal argument exception if we attempt to push bytes onto the stack when there are none" in {
    val stack = List()
    val script = List(OP_PUSHDATA1)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    intercept[IllegalArgumentException] {
      val newProgram = pushScriptNumberBytesToStack(program)
    }


  }
}
