package org.scalacoin.script.arithmetic

import org.scalacoin.script.{ScriptProgramFactory, ScriptProgramImpl}
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
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opAdd(program)
    newProgram.stack.head must be (ScriptNumberImpl(3))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_ADD correctly on ScriptConstantImpl" in {
    //0x64 is the hexadecimal representation for 100
    val stack = List(ScriptConstantImpl("64"), ScriptConstantImpl("64"))
    val script = List(OP_ADD)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opAdd(program)
    //0xC8 is 200 in hex
    newProgram.stack.head must be (ScriptNumberImpl(200))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_ADD correctly on a ScriptConstant & ScriptNumber that are used as the args" in {
    val stack = List(ScriptNumberImpl(1), ScriptConstantImpl("64"))
    val script = List(OP_ADD)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram  = opAdd(program)
    //0x65 is 101 in hex
    newProgram.stack.head must be (ScriptNumberImpl(101))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_ADD correctly on a a negative number" in {
    val stack = List(ScriptConstantImpl("3e8"), ScriptNumberImpl(-1))
    val script = List(OP_ADD)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opAdd(program)

    newProgram.stack.head must be (ScriptNumberImpl(999))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_1ADD correctly" in {
    val stack = List(ScriptNumberImpl(0))
    val script = List(OP_1ADD)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = op1Add(program)

    newProgram.stack.head must be (ScriptNumberImpl(1))
    newProgram.script.isEmpty must be (true)
  }

  it must "throw an exception if we have an OP_1ADD with nothing on the stack" in {
    intercept[IllegalArgumentException] {
      val stack = List()
      val script = List(OP_1ADD)
      val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
      val newProgram = op1Add(program)
    }
  }
  it must "throw an exception if we have an OP_1ADD with a non script number on the stack" in {
    intercept[IllegalArgumentException] {
      val stack = List(ScriptConstantImpl("nan"))
      val script = List(OP_1ADD)
      val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
      val newProgram = op1Add(program)
    }
  }
  it must "perform an OP_1SUB corectly" in {
    val stack = List(ScriptNumberImpl(0))
    val script = List(OP_1SUB)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = op1Sub(program)

    newProgram.stack.head must be (ScriptNumberImpl(-1))
    newProgram.script.isEmpty must be (true)
  }

  it must "throw an exception if we have an OP_1SUB with nothing on the stack" in {
    intercept[IllegalArgumentException] {
      val stack = List()
      val script = List(OP_1SUB)
      val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
      val newProgram = op1Sub(program)
    }
  }

  it must "throw an exception if we have an OP_1SUB with a non script number on the stack" in {
    intercept[IllegalArgumentException] {
      val stack = List(ScriptConstantImpl("nan"))
      val script = List(OP_1SUB)
      val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
      val newProgram = op1Sub(program)
    }
  }
  it must "perform an OP_SUB corectly" in {
    val stack = List(ScriptNumberImpl(1),ScriptNumberImpl(0))
    val script = List(OP_SUB)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opSub(program)

    newProgram.stack.head must be (ScriptNumberImpl(-1))
    newProgram.script.isEmpty must be (true)
  }

  it must "throw an exception if we have an OP_SUB with nothing on the stack" in {
    intercept[IllegalArgumentException] {
      val stack = List()
      val script = List(OP_SUB)
      val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
      val newProgram = opSub(program)
    }
  }

  it must "throw an exception if we have an OP_SUB with one of the two numbers a non script number on the stack" in {
    intercept[IllegalArgumentException] {
      val stack = List(ScriptNumberImpl(1), ScriptConstantImpl("nan"))
      val script = List(OP_SUB)
      val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
      val newProgram = opSub(program)
    }
  }

  it must "perform an OP_ABS on a negative number corectly" in {
    val stack = List(ScriptNumberImpl(-1))
    val script = List(OP_ABS)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opAbs(program)

    newProgram.stack.head must be (ScriptNumberImpl(1))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform OP_ABS on zero correctly" in {
    val stack = List(ScriptNumberImpl(0))
    val script = List(OP_ABS)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opAbs(program)

    newProgram.stack.head must be (ScriptNumberImpl(0))
    newProgram.script.isEmpty must be (true)
  }
  it must "throw an exception if we have an OP_ABS with nothing on the stack" in {
    intercept[IllegalArgumentException] {
      val stack = List()
      val script = List(OP_ABS)
      val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
      val newProgram = opAbs(program)
    }
  }

  it must "throw an exception if we have an OP_ABS with a non script number on the stack" in {
    intercept[IllegalArgumentException] {
      val stack = List(ScriptConstantImpl("nan"))
      val script = List(OP_ABS)
      val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
      val newProgram = opAbs(program)
    }
  }
  it must "perform an OP_NEGATE on a zero correctly" in {
    val stack = List(ScriptNumberImpl(0))
    val script = List(OP_NEGATE)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opNegate(program)

    newProgram.stack.head must be (ScriptNumberImpl(0))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_NEGATE on a positive number correctly" in {
    val stack = List(ScriptNumberImpl(1))
    val script = List(OP_NEGATE)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opNegate(program)

    newProgram.stack.head must be (ScriptNumberImpl(-1))
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_NEGATE on a negative number correctly" in {
    val stack = List(ScriptNumberImpl(-1))
    val script = List(OP_NEGATE)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opNegate(program)

    newProgram.stack.head must be (ScriptNumberImpl(1))
    newProgram.script.isEmpty must be (true)
  }
  it must "throw an exception if we have an OP_NEGATE with nothing on the stack" in {
    intercept[IllegalArgumentException] {
      val stack = List()
      val script = List(OP_NEGATE)
      val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
      val newProgram = opNegate(program)
    }
  }

  it must "throw an exception if we have an OP_NEGATE with a non script number on the stack" in {
    intercept[IllegalArgumentException] {
      val stack = List(ScriptConstantImpl("nan"))
      val script = List(OP_NEGATE)
      val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
      val newProgram = opNegate(program)
    }
  }
  it must "perform an OP_NOT correctly where 0 is the stack top" in {
    val stack = List(ScriptNumberImpl(0))
    val script = List(OP_NOT)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opNot(program)

    newProgram.stack.head must be (OP_1)
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_NOT correctly where 1 is the stack top" in {
    val stack = List(ScriptNumberImpl(1))
    val script = List(OP_NOT)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opNot(program)

    newProgram.stack.head must be (OP_0)
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_0NOTEQUAL correctly where 0 is the stack top" in {
    val stack = List(ScriptNumberImpl(0))
    val script = List(OP_0NOTEQUAL)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = op0NotEqual(program)

    newProgram.stack.head must be (OP_0)
    newProgram.script.isEmpty must be (true)
  }

  it must "perform an OP_0NOTEQUAL correctly where 1 is the stack top" in {
    val stack = List(ScriptNumberImpl(1))
    val script = List(OP_0NOTEQUAL)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = op0NotEqual(program)

    newProgram.stack.head must be (OP_1)
    newProgram.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLAND correctly for two OP_0s" in {
    val stack = List(ScriptNumberImpl(0), ScriptNumberImpl(0))
    val script = List(OP_BOOLAND)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opBoolAnd(program)

    newProgram.stack.head must be (OP_1)
    newProgram.script.isEmpty must be (true)

    val stack1 = List(OP_0, OP_0)
    val script1 = List(OP_BOOLAND)
    val program1 = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram1 = opBoolAnd(program)

    newProgram1.stack.head must be (OP_1)
    newProgram1.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLAND correctly for one OP_0" in {
    val stack = List(ScriptNumberImpl(0), OP_1)
    val script = List(OP_BOOLAND)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opBoolAnd(program)

    newProgram.stack.head must be (OP_0)
    newProgram.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLAND correctly for zero OP_0" in {
    val stack = List(OP_1, OP_1)
    val script = List(OP_BOOLAND)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opBoolAnd(program)

    newProgram.stack.head must be (OP_0)
    newProgram.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLOR correctly for two OP_1s" in {
    val stack = List(OP_1, OP_1)
    val script = List(OP_BOOLOR)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opBoolOr(program)

    newProgram.stack.head must be (OP_1)
    newProgram.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLOR correctly for two OP_0s" in {
    val stack = List(OP_0, OP_0)
    val script = List(OP_BOOLOR)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opBoolOr(program)

    newProgram.stack.head must be (OP_0)
    newProgram.script.isEmpty must be (true)
  }

  it must "have an OP_BOOLOR correctly for one OP_0" in {
    val stack = List(OP_0, OP_1)
    val script = List(OP_BOOLOR)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opBoolOr(program)

    newProgram.stack.head must be (OP_1)
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_NUMEQUAL correctly" in {
    val stack = List(OP_0, ScriptNumberImpl(0))
    val script = List(OP_NUMEQUAL)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opNumEqual(program)

    newProgram.stack.head must be (OP_1)
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_NUMNOTEQUAL for two numbers that are the same" in {
    val stack = List(OP_0, ScriptNumberImpl(0))
    val script = List(OP_NUMNOTEQUAL)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opNumNotEqual(program)

    newProgram.stack.head must be (OP_0)
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_NUMNOTEQUAL for two numbers that are not the same" in {
    val stack = List(OP_0, ScriptNumberImpl(1))
    val script = List(OP_NUMNOTEQUAL)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opNumNotEqual(program)

    newProgram.stack.head must be (OP_1)
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_LESSTHAN correctly" in  {
    val stack = List(OP_0, ScriptNumberImpl(1))
    val script = List(OP_LESSTHAN)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opLessThan(program)

    newProgram.stack.head must be (OP_0)
    newProgram.script.isEmpty must be (true)

    val stack1 = List(OP_0, ScriptNumberImpl(0))
    val script1 = List(OP_LESSTHAN)
    ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val program1 = ScriptProgramFactory.factory(TestUtil.testProgram,stack1,script1)
    val newProgram1 = opLessThan(program1)

    newProgram1.stack.head must be (OP_0)
    newProgram1.script.isEmpty must be (true)

    val stack2 = List(OP_1, ScriptNumberImpl(0))
    val script2 = List(OP_LESSTHAN)
    val program2 = ScriptProgramFactory.factory(TestUtil.testProgram, stack2,script2)
    val newProgram2 = opLessThan(program2)

    newProgram2.stack.head must be (OP_1)
    newProgram2.script.isEmpty must be (true)
  }

  it must "evaluate an OP_GREATERTHAN correctly" in  {
    val stack = List(OP_0, ScriptNumberImpl(1))
    val script = List(OP_GREATERTHAN)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opGreaterThan(program)

    newProgram.stack.head must be (OP_1)
    newProgram.script.isEmpty must be (true)

    val stack1 = List(OP_0, ScriptNumberImpl(0))
    val script1 = List(OP_GREATERTHAN)
    val program1 = ScriptProgramFactory.factory(TestUtil.testProgram,stack1,script1)
    val newProgram1 = opGreaterThan(program1)

    newProgram1.stack.head must be (OP_0)
    newProgram1.script.isEmpty must be (true)

    val stack2 = List(OP_1, ScriptNumberImpl(0))
    val script2 = List(OP_GREATERTHAN)
    val program2 = ScriptProgramFactory.factory(TestUtil.testProgram, stack2,script2)
    val newProgram2 = opGreaterThan(program2)

    newProgram2.stack.head must be (OP_0)
    newProgram2.script.isEmpty must be (true)
  }


  it must "evaluate an OP_MIN correctly" in {
    val stack = List(OP_0, ScriptNumberImpl(1))
    val script = List(OP_MIN)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opMin(program)

    newProgram.stack must be (List(OP_0))
  }

  it must "evaluate an OP_MAX correctly" in {
    val stack = List(OP_0, ScriptNumberImpl(1))
    val script = List(OP_MAX)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opMax(program)

    newProgram.stack must be (List(ScriptNumberImpl(1)))
  }

  it must "evaluate an OP_WITHIN correctly" in {
    val stack = List(OP_0,ScriptNumberImpl(2), ScriptNumberImpl(1))
    val script = List(OP_WITHIN)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opWithin(program)
    newProgram.stack must be (List(OP_0))
    newProgram.script.isEmpty must be (true)


    val stack1 = List(OP_0, ScriptNumberImpl(1),ScriptNumberImpl(0))
    val script1 = List(OP_WITHIN)
    val program1 = ScriptProgramFactory.factory(TestUtil.testProgram, stack1,script1)
    val newProgram1 = opWithin(program1)
    newProgram1.stack must be (List(OP_1))
    newProgram1.script.isEmpty must be (true)
  }


}
