package org.scalacoin.script.arithmetic

import org.scalacoin.script.{ScriptProgram}
import org.scalacoin.script.constant._
import org.scalacoin.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/25/16.
 */
class ArithmeticInterpreterTest extends FlatSpec with MustMatchers with ArithmeticInterpreter {

  "ArithmeticInterpreter" must "perform an OP_ADD correctly" in {
    val stack = List(ScriptNumberFactory.one, ScriptNumberFactory.fromNumber(2))
    val script = List(OP_ADD)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opAdd(program)
    newProgram.stack.head must be (ScriptNumberFactory.fromNumber(3))
    newProgram.script.isEmpty must be (true)
  }


  it must "perform an OP_1ADD correctly" in {
    val stack = List(ScriptNumberFactory.zero)
    val script = List(OP_1ADD)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = op1Add(program)

    newProgram.stack.head must be (ScriptNumberFactory.one)
    newProgram.script.isEmpty must be (true)
  }

  it must "mark the script as invalid if we have an OP_1ADD with nothing on the stack" in {

    val stack = List()
    val script = List(OP_1ADD)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = op1Add(program)
    newProgram.isValid must be (false)

  }

  it must "perform an OP_1SUB corectly" in {
    val stack = List(ScriptNumberFactory.zero)
    val script = List(OP_1SUB)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = op1Sub(program)

    newProgram.stack.head must be (ScriptNumberFactory.fromNumber(-1))
    newProgram.script.isEmpty must be (true)
  }

  it must "mark a script as invalid if we have an OP_1SUB with nothing on the stack" in {

    val stack = List()
    val script = List(OP_1SUB)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = op1Sub(program)
    newProgram.isValid must be (false)
  }

  it must "perform an OP_SUB corectly" in {
    val stack = List(ScriptNumberFactory.one,ScriptNumberFactory.zero)
    val script = List(OP_SUB)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opSub(program)

    newProgram.stack.head must be (ScriptNumberImpl(-1))
    newProgram.script.isEmpty must be (true)
  }

  it must "mark a script as invalid if we have an OP_SUB with nothing on the stack" in {
    val stack = List()
    val script = List(OP_SUB)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opSub(program)
    newProgram.isValid must be (false)
  }

  it must "perform an OP_ABS on a negative number corectly" in {
    val stack = List(ScriptNumberFactory.fromNumber(-1))
    val script = List(OP_ABS)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opAbs(program)

    newProgram.stack.head must be (ScriptNumberFactory.one)
    newProgram.script.isEmpty must be (true)
  }

  it must "perform OP_ABS on zero correctly" in {
    val stack = List(ScriptNumberFactory.zero)
    val script = List(OP_ABS)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opAbs(program)

    newProgram.stack.head must be (ScriptNumberFactory.zero)
    newProgram.script.isEmpty must be (true)
  }
  it must "mark a script as invalid if we have an OP_ABS with nothing on the stack" in {
    val stack = List()
    val script = List(OP_ABS)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opAbs(program)
    newProgram.isValid must be (false)
  }

  it must "perform an OP_NEGATE on a zero correctly" in {
    val stack = List(ScriptNumberFactory.zero)
    val script = List(OP_NEGATE)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opNegate(program)

    newProgram.stack.head must be (ScriptNumberFactory.zero)
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_NEGATE on a positive number correctly" in {
    val stack = List(ScriptNumberFactory.one)
    val script = List(OP_NEGATE)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opNegate(program)

    newProgram.stack.head must be (ScriptNumberFactory.fromNumber(-1))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_NEGATE on a negative number correctly" in {
    val stack = List(ScriptNumberFactory.fromNumber(-1))
    val script = List(OP_NEGATE)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opNegate(program)

    newProgram.stack.head must be (ScriptNumberFactory.one)
    newProgram.script.isEmpty must be (true)
  }
  it must "mark a script as invalid if we have an OP_NEGATE with nothing on the stack" in {

    val stack = List()
    val script = List(OP_NEGATE)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opNegate(program)
    newProgram.isValid must be (false)
  }

  it must "perform an OP_NOT correctly where 0 is the stack top" in {
    val stack = List(ScriptNumberFactory.zero)
    val script = List(OP_NOT)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opNot(program)

    newProgram.stackTopIsTrue must be (true)
    newProgram.stack.head must be (OP_TRUE)
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_NOT correctly where 1 is the stack top" in {
    val stack = List(ScriptNumberFactory.one)
    val script = List(OP_NOT)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opNot(program)

    newProgram.stackTopIsFalse must be (true)
    newProgram.stack.head must be (OP_FALSE)
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_0NOTEQUAL correctly where 0 is the stack top" in {
    val stack = List(ScriptNumberFactory.zero)
    val script = List(OP_0NOTEQUAL)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = op0NotEqual(program)

    newProgram.stack.head must be (OP_FALSE)
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_0NOTEQUAL correctly where 1 is the stack top" in {
    val stack = List(ScriptNumberFactory.one)
    val script = List(OP_0NOTEQUAL)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = op0NotEqual(program)

    newProgram.stack.head must be (OP_TRUE)
    newProgram.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLAND correctly for two OP_0s" in {
    val stack = List(ScriptNumberFactory.zero, ScriptNumberFactory.zero)
    val script = List(OP_BOOLAND)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opBoolAnd(program)

    newProgram.stackTopIsFalse must be (true)
    newProgram.stack.head must be (OP_FALSE)
    newProgram.script.isEmpty must be (true)

    val stack1 = List(OP_0, OP_0)
    val script1 = List(OP_BOOLAND)
    val program1 = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram1 = opBoolAnd(program)

    newProgram.stackTopIsFalse must be (true)
    newProgram1.stack.head must be (OP_FALSE)
    newProgram1.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLAND correctly for one OP_0" in {
    val stack = List(ScriptNumberFactory.zero, OP_1)
    val script = List(OP_BOOLAND)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opBoolAnd(program)

    newProgram.stackTopIsTrue must be (false)
    newProgram.stack.head must be (OP_FALSE)
    newProgram.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLAND correctly for zero OP_0" in {
    val stack = List(OP_1, OP_1)
    val script = List(OP_BOOLAND)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opBoolAnd(program)

    newProgram.stackTopIsTrue must be (true)
    newProgram.stack.head must be (OP_TRUE)
    newProgram.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLOR correctly for two OP_1s" in {
    val stack = List(OP_1, OP_1)
    val script = List(OP_BOOLOR)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opBoolOr(program)

    newProgram.stack.head must be (OP_TRUE)
    newProgram.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLOR correctly for two OP_0s" in {
    val stack = List(OP_0, OP_0)
    val script = List(OP_BOOLOR)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opBoolOr(program)

    newProgram.stack.head must be (OP_FALSE)
    newProgram.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLOR correctly for one OP_0" in {
    val stack = List(OP_0, OP_1)
    val script = List(OP_BOOLOR)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opBoolOr(program)

    newProgram.stack.head must be (OP_TRUE)
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_NUMEQUAL correctly" in {
    val stack = List(OP_0, ScriptNumberFactory.zero)
    val script = List(OP_NUMEQUAL)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opNumEqual(program)

    newProgram.stack.head must be (OP_TRUE)
    newProgram.script.isEmpty must be (true)
  }

  it must "evaulate an OP_NUMEQUAL for two OP_0" in {
    val stack = List(OP_0, OP_0)
    val script = List(OP_NUMEQUAL)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opNumEqual(program)

    newProgram.stackTopIsTrue must be (true)
    newProgram.stack.head must be (OP_TRUE)
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_NUMNOTEQUAL for two numbers that are the same" in {
    val stack = List(OP_0, ScriptNumberFactory.zero)
    val script = List(OP_NUMNOTEQUAL)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opNumNotEqual(program)

    newProgram.stack.head must be (OP_FALSE)
    newProgram.script.isEmpty must be (true)
  }



  it must "evaluate an OP_NUMNOTEQUAL for two numbers that are not the same" in {
    val stack = List(OP_0, ScriptNumberFactory.one)
    val script = List(OP_NUMNOTEQUAL)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opNumNotEqual(program)

    newProgram.stack.head must be (OP_TRUE)
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_LESSTHAN correctly" in  {
    val stack = List(OP_0, ScriptNumberFactory.one)
    val script = List(OP_LESSTHAN)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opLessThan(program)

    newProgram.stack.head must be (OP_FALSE)
    newProgram.script.isEmpty must be (true)

    val stack1 = List(OP_0, ScriptNumberFactory.zero)
    val script1 = List(OP_LESSTHAN)
    ScriptProgram(TestUtil.testProgram, stack,script)
    val program1 = ScriptProgram(TestUtil.testProgram,stack1,script1)
    val newProgram1 = opLessThan(program1)

    newProgram1.stack.head must be (OP_FALSE)
    newProgram1.script.isEmpty must be (true)

    val stack2 = List(OP_1, ScriptNumberImpl(0))
    val script2 = List(OP_LESSTHAN)
    val program2 = ScriptProgram(TestUtil.testProgram, stack2,script2)
    val newProgram2 = opLessThan(program2)

    newProgram2.stack.head must be (OP_TRUE)
    newProgram2.script.isEmpty must be (true)
  }

  it must "evaluate an OP_GREATERTHAN correctly" in  {
    val stack = List(OP_0, ScriptNumberFactory.one)
    val script = List(OP_GREATERTHAN)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opGreaterThan(program)

    newProgram.stack.head must be (OP_TRUE)
    newProgram.script.isEmpty must be (true)

    val stack1 = List(OP_0, ScriptNumberFactory.zero)
    val script1 = List(OP_GREATERTHAN)
    val program1 = ScriptProgram(TestUtil.testProgram,stack1,script1)
    val newProgram1 = opGreaterThan(program1)

    newProgram1.stack.head must be (OP_FALSE)
    newProgram1.script.isEmpty must be (true)

    val stack2 = List(OP_1, ScriptNumberFactory.zero)
    val script2 = List(OP_GREATERTHAN)
    val program2 = ScriptProgram(TestUtil.testProgram, stack2,script2)
    val newProgram2 = opGreaterThan(program2)

    newProgram2.stack.head must be (OP_FALSE)
    newProgram2.script.isEmpty must be (true)
  }


  it must "evaluate an OP_MIN correctly" in {
    val stack = List(OP_0, ScriptNumberFactory.one)
    val script = List(OP_MIN)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opMin(program)

    newProgram.stack must be (List(OP_0))
  }

  it must "evaluate an OP_MAX correctly" in {
    val stack = List(OP_0, ScriptNumberFactory.one)
    val script = List(OP_MAX)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opMax(program)

    newProgram.stack must be (List(ScriptNumberFactory.one))
  }

  it must "evaluate an OP_WITHIN correctly" in {
    val stack = List(ScriptNumberFactory.fromNumber(2), ScriptNumberFactory.one, OP_0)
    val script = List(OP_WITHIN)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opWithin(program)
    newProgram.stack must be (List(OP_FALSE))
    newProgram.script.isEmpty must be (true)


    val stack1 = List(ScriptNumberFactory.one, OP_0, ScriptNumberFactory.zero)
    val script1 = List(OP_WITHIN)
    val program1 = ScriptProgram(TestUtil.testProgram, stack1,script1)
    val newProgram1 = opWithin(program1)
    newProgram1.stack must be (List(OP_TRUE))
    newProgram1.script.isEmpty must be (true)
  }


  it must "interpret two script constants as numbers and then add them" in {
    val scriptConstant1 = ScriptConstantFactory.fromHex("ffffffff")
    val scriptConstant2 = ScriptConstantFactory.fromHex("ffffff7f")
    val stack = List(scriptConstant1, scriptConstant2)
    val script = List(OP_ADD)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = opAdd(program)
    newProgram.stack must be (List(ScriptNumberFactory.zero))
    newProgram.script.isEmpty must be (true)
  }


}
