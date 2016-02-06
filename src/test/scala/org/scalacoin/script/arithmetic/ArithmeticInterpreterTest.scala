package org.scalacoin.script.arithmetic

import org.scalacoin.script.ScriptProgramImpl
import org.scalacoin.script.constant.{OP_0, OP_1, ScriptConstantImpl, ScriptNumberImpl}
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

  it must "perform an OP_1ADD correctly" in {
    val stack = List(ScriptNumberImpl(0))
    val script = List(OP_1ADD)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = op1Add(program)

    newProgram.stack.head must be (ScriptNumberImpl(1))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_1SUB corectly" in {
    val stack = List(ScriptNumberImpl(0))
    val script = List(OP_1SUB)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = op1Sub(program)

    newProgram.stack.head must be (ScriptNumberImpl(-1))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_SUB corectly" in {
    val stack = List(ScriptNumberImpl(1),ScriptNumberImpl(0))
    val script = List(OP_SUB)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opSub(program)

    newProgram.stack.head must be (ScriptNumberImpl(-1))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_ABS on a negative number corectly" in {
    val stack = List(ScriptNumberImpl(-1))
    val script = List(OP_ABS)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opAbs(program)

    newProgram.stack.head must be (ScriptNumberImpl(1))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform OP_ABS on zero correctly" in {
    val stack = List(ScriptNumberImpl(0))
    val script = List(OP_ABS)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opAbs(program)

    newProgram.stack.head must be (ScriptNumberImpl(0))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_NEGATE on a zero correctly" in {
    val stack = List(ScriptNumberImpl(0))
    val script = List(OP_NEGATE)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opNegate(program)

    newProgram.stack.head must be (ScriptNumberImpl(0))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_NEGATE on a positive number correctly" in {
    val stack = List(ScriptNumberImpl(1))
    val script = List(OP_NEGATE)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opNegate(program)

    newProgram.stack.head must be (ScriptNumberImpl(-1))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_NEGATE on a negative number correctly" in {
    val stack = List(ScriptNumberImpl(-1))
    val script = List(OP_NEGATE)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opNegate(program)

    newProgram.stack.head must be (ScriptNumberImpl(1))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_NOT correctly where 0 is the stack top" in {
    val stack = List(ScriptNumberImpl(0))
    val script = List(OP_NOT)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opNot(program)

    newProgram.stack.head must be (OP_1)
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_NOT correctly where 1 is the stack top" in {
    val stack = List(ScriptNumberImpl(1))
    val script = List(OP_NOT)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opNot(program)

    newProgram.stack.head must be (OP_0)
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_0NOTEQUAL correctly where 0 is the stack top" in {
    val stack = List(ScriptNumberImpl(0))
    val script = List(OP_0NOTEQUAL)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = op0NotEqual(program)

    newProgram.stack.head must be (OP_0)
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_0NOTEQUAL correctly where 1 is the stack top" in {
    val stack = List(ScriptNumberImpl(1))
    val script = List(OP_0NOTEQUAL)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = op0NotEqual(program)

    newProgram.stack.head must be (OP_1)
    newProgram.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLAND correctly for two OP_0s" in {
    val stack = List(ScriptNumberImpl(0), ScriptNumberImpl(0))
    val script = List(OP_BOOLAND)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opBoolAnd(program)

    newProgram.stack.head must be (OP_1)
    newProgram.script.isEmpty must be (true)

    val stack1 = List(OP_0, OP_0)
    val script1 = List(OP_BOOLAND)
    val program1 = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram1 = opBoolAnd(program)

    newProgram1.stack.head must be (OP_1)
    newProgram1.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLAND correctly for one OP_0" in {
    val stack = List(ScriptNumberImpl(0), OP_1)
    val script = List(OP_BOOLAND)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opBoolAnd(program)

    newProgram.stack.head must be (OP_0)
    newProgram.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLAND correctly for zero OP_0" in {
    val stack = List(OP_1, OP_1)
    val script = List(OP_BOOLAND)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opBoolAnd(program)

    newProgram.stack.head must be (OP_0)
    newProgram.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLOR correctly for two OP_1s" in {
    val stack = List(OP_1, OP_1)
    val script = List(OP_BOOLOR)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opBoolOr(program)

    newProgram.stack.head must be (OP_1)
    newProgram.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLOR correctly for two OP_0s" in {
    val stack = List(OP_0, OP_0)
    val script = List(OP_BOOLOR)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opBoolOr(program)

    newProgram.stack.head must be (OP_0)
    newProgram.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLOR correctly for one OP_0" in {
    val stack = List(OP_0, OP_1)
    val script = List(OP_BOOLOR)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opBoolOr(program)

    newProgram.stack.head must be (OP_1)
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_NUMEQUAL correctly" in {
    val stack = List(OP_0, ScriptNumberImpl(0))
    val script = List(OP_NUMEQUAL)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opNumEqual(program)

    newProgram.stack.head must be (OP_1)
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_NUMNOTEQUAL for two numbers that are the same" in {
    val stack = List(OP_0, ScriptNumberImpl(0))
    val script = List(OP_NUMNOTEQUAL)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opNumNotEqual(program)

    newProgram.stack.head must be (OP_0)
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_NUMNOTEQUAL for two numbers that are not the same" in {
    val stack = List(OP_0, ScriptNumberImpl(1))
    val script = List(OP_NUMNOTEQUAL)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opNumNotEqual(program)

    newProgram.stack.head must be (OP_1)
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_LESSTHAN correctly" in  {
    val stack = List(OP_0, ScriptNumberImpl(1))
    val script = List(OP_LESSTHAN)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opLessThan(program)

    newProgram.stack.head must be (OP_0)
    newProgram.script.isEmpty must be (true)

    val stack1 = List(OP_0, ScriptNumberImpl(0))
    val script1 = List(OP_LESSTHAN)
    val program1 = ScriptProgramImpl(stack1,script1,TestUtil.transaction,List())
    val newProgram1 = opLessThan(program1)

    newProgram1.stack.head must be (OP_0)
    newProgram1.script.isEmpty must be (true)

    val stack2 = List(OP_1, ScriptNumberImpl(0))
    val script2 = List(OP_LESSTHAN)
    val program2 = ScriptProgramImpl(stack2,script2,TestUtil.transaction,List())
    val newProgram2 = opLessThan(program2)

    newProgram2.stack.head must be (OP_1)
    newProgram2.script.isEmpty must be (true)
  }

  it must "evaluate an OP_GREATERTHAN correctly" in  {
    val stack = List(OP_0, ScriptNumberImpl(1))
    val script = List(OP_GREATERTHAN)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction,List())
    val newProgram = opGreaterThan(program)

    newProgram.stack.head must be (OP_1)
    newProgram.script.isEmpty must be (true)

    val stack1 = List(OP_0, ScriptNumberImpl(0))
    val script1 = List(OP_GREATERTHAN)
    val program1 = ScriptProgramImpl(stack1,script1,TestUtil.transaction,List())
    val newProgram1 = opGreaterThan(program1)

    newProgram1.stack.head must be (OP_0)
    newProgram1.script.isEmpty must be (true)

    val stack2 = List(OP_1, ScriptNumberImpl(0))
    val script2 = List(OP_GREATERTHAN)
    val program2 = ScriptProgramImpl(stack2,script2,TestUtil.transaction,List())
    val newProgram2 = opGreaterThan(program2)

    newProgram2.stack.head must be (OP_0)
    newProgram2.script.isEmpty must be (true)
  }
}
