package org.scalacoin.script.constant

import org.scalacoin.script.ScriptProgram
import org.scalacoin.script.bitwise.OP_EQUAL
import org.scalacoin.script.error.ScriptErrorInvalidStackOperation
import org.scalacoin.util.{ScriptProgramTestUtil, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/24/16.
 */
class ConstantInterpreterTest extends FlatSpec with MustMatchers with ConstantInterpreter {

  "ConstantInterpreter" must "interpret OP_PUSHDATA1 correctly" in {
    val stack = List()
    val script = List(OP_PUSHDATA1, ScriptNumberFactory.one, ScriptNumberFactory.fromNumber(8), OP_7, OP_EQUAL)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opPushData1(program)
    newProgram.stack must be (List(ScriptNumberFactory.fromNumber(8)))
    newProgram.script must be (List(OP_7,OP_EQUAL))
  }

  it must "interpret OP_PUSHDATA2 correctly" in {
    val stack = List()
    val script = List(OP_PUSHDATA2, ScriptNumberFactory.fromHex("0100"), ScriptNumberFactory.fromNumber(8), OP_8, OP_EQUAL)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opPushData2(program)
    newProgram.stack must be (List(ScriptNumberFactory.fromNumber(8)))
    newProgram.script must be (List(OP_8,OP_EQUAL))
  }

  it must "interpret OP_PUSHDATA4 correctly" in {
    val stack = List()
    val script = List(OP_PUSHDATA4, ScriptNumberFactory.fromHex("01000000"), ScriptNumberFactory.fromNumber(9), OP_9, OP_EQUAL)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opPushData4(program)
    newProgram.stack must be (List(ScriptNumberFactory.fromNumber(9)))
    newProgram.script must be (List(OP_9, OP_EQUAL))
  }


  it must "push a constant 2 bytes onto the stack" in {
    val stack = List()
    val script = List(BytesToPushOntoStackFactory.fromNumber(2).get, ScriptNumberFactory.one, OP_0)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = pushScriptNumberBytesToStack(program)
    newProgram.script.isEmpty must be (true)
    newProgram.stack must be (List(ScriptConstantFactory.fromHex("0100")))
  }

  it must "push 0 bytes onto the stack which is OP_0" in {
    val stack = List()
    val script = List(OP_PUSHDATA1,BytesToPushOntoStackFactory.fromNumber(0).get)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram  = opPushData1(program)
    newProgram.stackTopIsFalse must be (true)
    newProgram.stack must be (List(ScriptNumberFactory.zero))

    val stack1 = List()
    val script1 = List(OP_PUSHDATA2,BytesToPushOntoStackFactory.fromNumber(0).get)
    val program1 = ScriptProgram(TestUtil.testProgram, stack1,script1)
    val newProgram1  = opPushData2(program1)
    newProgram1.stack must be (List(ScriptNumberFactory.zero))

    val stack2 = List()
    val script2 = List(OP_PUSHDATA4,BytesToPushOntoStackFactory.fromNumber(0).get)
    val program2 = ScriptProgram(TestUtil.testProgram, stack2,script2)
    val newProgram2 = opPushData4(program2)
    newProgram2.stack must be (List(ScriptNumberFactory.zero))
  }


  it must "mark a program as invalid if we have do not have enough bytes to be pushed onto the stack by the push operation" in {
    val stack = List()
    val script = List(OP_PUSHDATA1,BytesToPushOntoStackFactory.factory(1).get)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    val newProgram  = ScriptProgramTestUtil.toExecutedScriptProgram(opPushData1(program))
    newProgram.error must be (Some(ScriptErrorInvalidStackOperation))
  }
}
