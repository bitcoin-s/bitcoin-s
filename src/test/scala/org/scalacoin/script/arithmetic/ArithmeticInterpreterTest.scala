package org.scalacoin.script.arithmetic

import org.scalacoin.script.ScriptProgramImpl
import org.scalacoin.script.constant.{ScriptConstantImpl, ScriptNumberImpl}
import org.scalacoin.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/25/16.
 */
class ArithmeticInterpreterTest extends FlatSpec with MustMatchers with ArithmeticInterpreter {

  "ArithmeticInterpreter" must "perform an OP_ADD correctly" in {
    val stack = List(ScriptNumberImpl(1), ScriptNumberImpl(2))
    val script = List(OP_ADD)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opAdd(program)
    newProgram.stack.head must be (ScriptNumberImpl(3))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_ADD correctly on ScriptConstantImpl" in {
    //0x64 is the hexadecimal representation for 100
    val stack = List(ScriptConstantImpl("64"), ScriptConstantImpl("64"))
    val script = List(OP_ADD)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opAdd(program)
    //0xC8 is 200 in hex
    newProgram.stack.head must be (ScriptNumberImpl(200))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_ADD correctly on a ScriptConstant & ScriptNumber that are used as the args" in {
    val stack = List(ScriptNumberImpl(1), ScriptConstantImpl("64"))
    val script = List(OP_ADD)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram  = opAdd(program)
    //0x65 is 101 in hex
    newProgram.stack.head must be (ScriptNumberImpl(101))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_ADD correctly on a a negative number" in {
    val stack = List(ScriptConstantImpl("3e8"), ScriptNumberImpl(-1))
    val script = List(OP_ADD)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opAdd(program)

    newProgram.stack.head must be (ScriptNumberImpl(999))
    newProgram.script.isEmpty must be (true)
  }
}
