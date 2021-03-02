package org.bitcoins.core.script.arithmetic

import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.result._
import org.bitcoins.core.script.{
  ExecutedScriptProgram,
  ExecutionInProgressScriptProgram
}
import org.bitcoins.core.util.ScriptProgramTestUtil
import org.bitcoins.testkitcore.util.TestUtil
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

import scala.util.Try

/** Created by chris on 1/25/16.
  */
class ArithmeticInterpreterTest extends BitcoinSUnitTest {

  val AI = ArithmeticInterpreter

  "ArithmeticInterpreter" must "perform an OP_ADD correctly" in {
    val stack = List(ScriptNumber.one, ScriptNumber(2))
    val script = List(OP_ADD)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opAdd(program)
    newProgram.stack.head must be(ScriptNumber(3))
    newProgram.script.isEmpty must be(true)
  }

  it must "perform an OP_1ADD correctly" in {
    val stack = List(ScriptNumber.zero)
    val script = List(OP_1ADD)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.op1Add(program)

    newProgram.stack.head must be(ScriptNumber.one)
    newProgram.script.isEmpty must be(true)
  }

  it must "mark the script as invalid if we have an OP_1ADD with nothing on the stack" in {

    val stack = List()
    val script = List(OP_1ADD)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(AI.op1Add(program))
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))

  }

  it must "perform an OP_1SUB correctly" in {
    val stack = List(ScriptNumber.zero)
    val script = List(OP_1SUB)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.op1Sub(program)

    newProgram.stack.head must be(ScriptNumber(-1))
    newProgram.script.isEmpty must be(true)
  }

  it must "mark a script as invalid if we have an OP_1SUB with nothing on the stack" in {

    val stack = List()
    val script = List(OP_1SUB)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(AI.op1Sub(program))
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))
  }

  it must "perform an OP_SUB correctly" in {
    val stack = List(ScriptNumber.one, ScriptNumber.zero)
    val script = List(OP_SUB)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opSub(program)

    newProgram.stack.head must be(ScriptNumber(-1))
    newProgram.script.isEmpty must be(true)
  }

  it must "mark a script as invalid if we have an OP_SUB with nothing on the stack" in {
    val stack = List()
    val script = List(OP_SUB)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(AI.opSub(program))
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))
  }

  it must "perform an OP_ABS on a negative number correctly" in {
    val stack = List(ScriptNumber(-1))
    val script = List(OP_ABS)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opAbs(program)

    newProgram.stack.head must be(ScriptNumber.one)
    newProgram.script.isEmpty must be(true)
  }

  it must "perform OP_ABS on zero correctly" in {
    val stack = List(ScriptNumber.zero)
    val script = List(OP_ABS)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opAbs(program)

    newProgram.stack.head must be(ScriptNumber.zero)
    newProgram.script.isEmpty must be(true)
  }
  it must "mark a script as invalid if we have an OP_ABS with nothing on the stack" in {
    val stack = List()
    val script = List(OP_ABS)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(AI.opAbs(program))
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))
  }

  it must "perform an OP_NEGATE on a zero correctly" in {
    val stack = List(ScriptNumber.zero)
    val script = List(OP_NEGATE)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opNegate(program)

    newProgram.stack.head must be(ScriptNumber.zero)
    newProgram.script.isEmpty must be(true)
  }

  it must "perform an OP_NEGATE on a positive number correctly" in {
    val stack = List(ScriptNumber.one)
    val script = List(OP_NEGATE)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opNegate(program)

    newProgram.stack.head must be(ScriptNumber(-1))
    newProgram.script.isEmpty must be(true)
  }

  it must "perform an OP_NEGATE on a negative number correctly" in {
    val stack = List(ScriptNumber(-1))
    val script = List(OP_NEGATE)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opNegate(program)

    newProgram.stack.head must be(ScriptNumber.one)
    newProgram.script.isEmpty must be(true)
  }
  it must "mark a script as invalid if we have an OP_NEGATE with nothing on the stack" in {

    val stack = List()
    val script = List(OP_NEGATE)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(AI.opNegate(program))
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))
  }

  it must "perform an OP_NOT correctly where 0 is the stack top" in {
    val stack = List(ScriptNumber.zero)
    val script = List(OP_NOT)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opNot(program)

    newProgram.stackTopIsTrue must be(true)
    newProgram.stack.head must be(OP_TRUE)
    newProgram.script.isEmpty must be(true)
  }

  it must "perform an OP_NOT correctly where 1 is the stack top" in {
    val stack = List(ScriptNumber.one)
    val script = List(OP_NOT)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opNot(program)

    newProgram.stackTopIsFalse must be(true)
    newProgram.stack.head must be(OP_FALSE)
    newProgram.script.isEmpty must be(true)
  }

  it must "perform an OP_0NOTEQUAL correctly where 0 is the stack top" in {
    val stack = List(ScriptNumber.zero)
    val script = List(OP_0NOTEQUAL)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.op0NotEqual(program)

    newProgram.stack.head must be(OP_FALSE)
    newProgram.script.isEmpty must be(true)
  }

  it must "perform an OP_0NOTEQUAL correctly where 1 is the stack top" in {
    val stack = List(ScriptNumber.one)
    val script = List(OP_0NOTEQUAL)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.op0NotEqual(program)

    newProgram.stack.head must be(OP_TRUE)
    newProgram.script.isEmpty must be(true)
  }

  it must "have an OP_BOOLAND correctly for two 0s" in {
    val stack = List(ScriptNumber.zero, ScriptNumber.zero)
    val script = List(OP_BOOLAND)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opBoolAnd(program)

    newProgram.stackTopIsFalse must be(true)
    newProgram.stack.head must be(OP_FALSE)
    newProgram.script.isEmpty must be(true)

    val newProgram1 = AI.opBoolAnd(program)

    newProgram.stackTopIsFalse must be(true)
    newProgram1.stack.head must be(OP_FALSE)
    newProgram1.script.isEmpty must be(true)
  }

  it must "have an OP_BOOLAND correctly for one 0" in {
    val stack = List(ScriptNumber.zero, OP_1)
    val script = List(OP_BOOLAND)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opBoolAnd(program)

    newProgram.stackTopIsTrue must be(false)
    newProgram.stack.head must be(OP_FALSE)
    newProgram.script.isEmpty must be(true)
  }

  it must "have an OP_BOOLOR correctly for two 1s" in {
    val stack = List(ScriptNumber.one, ScriptNumber.one)
    val script = List(OP_BOOLOR)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opBoolOr(program)

    newProgram.stack.head must be(OP_TRUE)
    newProgram.script.isEmpty must be(true)
  }

  it must "have an OP_BOOLOR correctly for two 0s" in {
    val stack = List(ScriptNumber.zero, ScriptNumber.zero)
    val script = List(OP_BOOLOR)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opBoolOr(program)

    newProgram.stack.head must be(OP_FALSE)
    newProgram.script.isEmpty must be(true)
  }

  it must "have an OP_BOOLOR correctly for one OP_0" in {
    val stack = List(ScriptNumber.zero, ScriptNumber.one)
    val script = List(OP_BOOLOR)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opBoolOr(program)

    newProgram.stack.head must be(OP_TRUE)
    newProgram.script.isEmpty must be(true)
  }

  it must "evaluate an OP_NUMEQUAL for two zeros" in {
    val stack = List(ScriptNumber.zero, ScriptNumber.zero)
    val script = List(OP_NUMEQUAL)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opNumEqual(program)

    newProgram.stackTopIsTrue must be(true)
    newProgram.stack.head must be(OP_TRUE)
    newProgram.script.isEmpty must be(true)
  }

  it must "evaluate an OP_NUMEQUALVERIFY for two zeros" in {
    val stack = List(ScriptNumber.zero, ScriptNumber.zero)
    val script = List(OP_NUMEQUALVERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opNumEqualVerify(program)
    newProgram.isInstanceOf[ExecutionInProgressScriptProgram] must be(true)
    newProgram.stack.isEmpty must be(true)
  }

  it must "evaluate an OP_NUMEQUALVERIFY for two different numbers" in {
    val stack = List(ScriptNumber.zero, ScriptNumber.one)
    val script = List(OP_NUMEQUALVERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opNumEqualVerify(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorVerify))
  }

  it must "mark the script as invalid for OP_NUMEQAULVERIFY without two stack elements" in {
    val stack = List(ScriptNumber.zero)
    val script = List(OP_NUMEQUALVERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opNumEqualVerify(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorInvalidStackOperation))
  }

  it must "evaluate an OP_NUMNOTEQUAL for two numbers that are the same" in {
    val stack = List(ScriptNumber.zero, ScriptNumber.zero)
    val script = List(OP_NUMNOTEQUAL)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opNumNotEqual(program)

    newProgram.stack.head must be(OP_FALSE)
    newProgram.script.isEmpty must be(true)
  }

  it must "evaluate an OP_NUMNOTEQUAL for two numbers that are not the same" in {
    val stack = List(ScriptNumber.zero, ScriptNumber.one)
    val script = List(OP_NUMNOTEQUAL)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opNumNotEqual(program)

    newProgram.stack.head must be(OP_TRUE)
    newProgram.script.isEmpty must be(true)
  }

  it must "evaluate an OP_LESSTHAN correctly" in {
    val stack = List(ScriptNumber.zero, ScriptNumber.one)
    val script = List(OP_LESSTHAN)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opLessThan(program)

    newProgram.stack.head must be(OP_FALSE)
    newProgram.script.isEmpty must be(true)

    val stack1 = List(ScriptNumber.zero, ScriptNumber.zero)
    val script1 = List(OP_LESSTHAN)
    TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack, script)
    val program1 =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack1,
                                                                   script1)
    val newProgram1 = AI.opLessThan(program1)

    newProgram1.stack.head must be(OP_FALSE)
    newProgram1.script.isEmpty must be(true)

    val stack2 = List(ScriptNumber.one, ScriptNumber.zero)
    val script2 = List(OP_LESSTHAN)
    val program2 =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack2,
                                                                   script2)
    val newProgram2 = AI.opLessThan(program2)

    newProgram2.stack.head must be(OP_TRUE)
    newProgram2.script.isEmpty must be(true)
  }

  it must "evaluate an OP_LESSTHANOREQUAL correctly" in {
    val stack = List(ScriptNumber.zero, ScriptNumber.one)
    val script = List(OP_LESSTHANOREQUAL)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opLessThanOrEqual(program)

    newProgram.stack.head must be(OP_FALSE)
    newProgram.script.isEmpty must be(true)

    val stack1 = List(ScriptNumber.zero, ScriptNumber.zero)
    val script1 = List(OP_LESSTHANOREQUAL)
    TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack, script)
    val program1 =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack1,
                                                                   script1)
    val newProgram1 = AI.opLessThanOrEqual(program1)

    newProgram1.stack.head must be(OP_TRUE)
    newProgram1.script.isEmpty must be(true)

    val stack2 = List(ScriptNumber.one, ScriptNumber.zero)
    val script2 = List(OP_LESSTHANOREQUAL)
    val program2 =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack2,
                                                                   script2)
    val newProgram2 = AI.opLessThanOrEqual(program2)

    newProgram2.stack.head must be(OP_TRUE)
    newProgram2.script.isEmpty must be(true)
  }

  it must "evaluate an OP_GREATERTHAN correctly" in {
    val stack = List(ScriptNumber.zero, ScriptNumber.one)
    val script = List(OP_GREATERTHAN)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opGreaterThan(program)

    newProgram.stack.head must be(OP_TRUE)
    newProgram.script.isEmpty must be(true)

    val stack1 = List(ScriptNumber.zero, ScriptNumber.zero)
    val script1 = List(OP_GREATERTHAN)
    val program1 =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack1,
                                                                   script1)
    val newProgram1 = AI.opGreaterThan(program1)

    newProgram1.stack.head must be(OP_FALSE)
    newProgram1.script.isEmpty must be(true)

    val stack2 = List(ScriptNumber.one, ScriptNumber.zero)
    val script2 = List(OP_GREATERTHAN)
    val program2 =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack2,
                                                                   script2)
    val newProgram2 = AI.opGreaterThan(program2)

    newProgram2.stack.head must be(OP_FALSE)
    newProgram2.script.isEmpty must be(true)
  }

  it must "evaluate an OP_GREATERTHANOREQUAL correctly" in {
    val stack = List(ScriptNumber.zero, ScriptNumber.one)
    val script = List(OP_GREATERTHANOREQUAL)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opGreaterThanOrEqual(program)

    newProgram.stack.head must be(OP_TRUE)
    newProgram.script.isEmpty must be(true)

    val stack1 = List(ScriptNumber.zero, ScriptNumber.zero)
    val script1 = List(OP_GREATERTHANOREQUAL)
    val program1 =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack1,
                                                                   script1)
    val newProgram1 = AI.opGreaterThanOrEqual(program1)

    newProgram1.stack.head must be(OP_TRUE)
    newProgram1.script.isEmpty must be(true)

    val stack2 = List(ScriptNumber.one, ScriptNumber.zero)
    val script2 = List(OP_GREATERTHANOREQUAL)
    val program2 =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack2,
                                                                   script2)
    val newProgram2 = AI.opGreaterThanOrEqual(program2)

    newProgram2.stack.head must be(OP_FALSE)
    newProgram2.script.isEmpty must be(true)
  }

  it must "evaluate an OP_MIN correctly" in {
    val stack = List(ScriptNumber.zero, ScriptNumber.one)
    val script = List(OP_MIN)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opMin(program)

    newProgram.stack must be(List(ScriptNumber.zero))
  }

  it must "mark a script invalid for OP_MIN if there isn't two elements on the stack" in {
    val stack = List(ScriptNumber.zero)
    val script = List(OP_MIN)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opMin(program)

    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorInvalidStackOperation))
  }

  it must "evaluate an OP_MAX correctly" in {
    val stack = List(ScriptNumber.zero, ScriptNumber.one)
    val script = List(OP_MAX)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opMax(program)

    newProgram.stack must be(List(ScriptNumber.one))
  }

  it must "mark a script invalid for OP_MAX if there isn't two elements on the stack" in {
    val stack = List(ScriptNumber.zero)
    val script = List(OP_MAX)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opMax(program)

    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorInvalidStackOperation))
  }

  it must "evaluate an OP_WITHIN correctly" in {
    val stack = List(ScriptNumber(2), ScriptNumber.one, ScriptNumber.zero)
    val script = List(OP_WITHIN)
    val program = TestUtil.testProgramExecutionInProgress
      .updateStackAndScript(stack, script)
      .removeFlags()
    val newProgram = AI.opWithin(program)
    newProgram.stack must be(List(OP_FALSE))
    newProgram.script.isEmpty must be(true)

    val stack1 = List(ScriptNumber.one, OP_0, ScriptNumber.zero)
    val script1 = List(OP_WITHIN)
    val program1 = TestUtil.testProgramExecutionInProgress
      .updateStackAndScript(stack1, script1)
      .removeFlags()
    val newProgram1 = AI.opWithin(program1)
    newProgram1.stack must be(List(OP_TRUE))
    newProgram1.script.isEmpty must be(true)
  }

  it must "mark the script as invalid if one of the numbers within OP_WITHIN is not encoded the smallest way possible" in {
    val stack = List(ScriptNumber("00"), ScriptNumber.one, ScriptNumber.one)
    val script = List(OP_WITHIN)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opWithin(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorUnknownError))
  }

  it must "mark the script as invalid for OP_WITHIN if one of the numbers is larger than 4 bytes" in {
    val stack =
      List(ScriptNumber("0000000000000000"), ScriptNumber.one, ScriptNumber.one)
    val script = List(OP_WITHIN)
    val program = TestUtil.testProgramExecutionInProgress
      .updateStackAndScript(stack, script)
      .removeFlags()
    val newProgram = AI.opWithin(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorUnknownError))
  }

  it must "mark the script as invalid for OP_WITHIN if we do not have 3 stack elements" in {
    val stack = List(ScriptNumber("0000000000000000"), ScriptNumber.one)
    val script = List(OP_WITHIN)
    val program = TestUtil.testProgramExecutionInProgress
      .updateStackAndScript(stack, script)
      .removeFlags()
    val newProgram = AI.opWithin(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorInvalidStackOperation))
  }

  it must "interpret two script constants as numbers and then add them" in {
    val scriptConstant1 = ScriptConstant("ffffffff")
    val scriptConstant2 = ScriptConstant("ffffff7f")
    val stack = List(scriptConstant1, scriptConstant2)
    val script = List(OP_ADD)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = AI.opAdd(program)
    newProgram.stack must be(List(ScriptNumber.zero))
    newProgram.script.isEmpty must be(true)
  }

  it must "fail to evaluate all OP codes if the script stack is empty" in {
    val stack = List(ScriptNumber.zero)
    val script = List()
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    Try(AI.opAdd(program)).isFailure must be(true)
    Try(AI.op1Add(program)).isFailure must be(true)
    Try(AI.op1Sub(program)).isFailure must be(true)
    Try(AI.opSub(program)).isFailure must be(true)
    Try(AI.opAbs(program)).isFailure must be(true)
    Try(AI.opNegate(program)).isFailure must be(true)
    Try(AI.opNot(program)).isFailure must be(true)
    Try(AI.op0NotEqual(program)).isFailure must be(true)
    Try(AI.opBoolAnd(program)).isFailure must be(true)
    Try(AI.opBoolOr(program)).isFailure must be(true)
    Try(AI.opNumEqual(program)).isFailure must be(true)
    Try(AI.opNumEqualVerify(program)).isFailure must be(true)
    Try(AI.opNumNotEqual(program)).isFailure must be(true)
    Try(AI.opLessThan(program)).isFailure must be(true)
    Try(AI.opLessThanOrEqual(program)).isFailure must be(true)
    Try(AI.opGreaterThan(program)).isFailure must be(true)
    Try(AI.opGreaterThanOrEqual(program)).isFailure must be(true)
    Try(AI.opMin(program)).isFailure must be(true)
    Try(AI.opMax(program)).isFailure must be(true)
    Try(AI.opWithin(program)).isFailure must be(true)
  }
}
