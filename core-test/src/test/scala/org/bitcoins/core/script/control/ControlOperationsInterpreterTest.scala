package org.bitcoins.core.script.control

import org.bitcoins.core.script.arithmetic.OP_ADD
import org.bitcoins.core.script.bitwise.OP_EQUAL
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.reserved.{OP_RESERVED, OP_VER}
import org.bitcoins.core.script.result.{
  ScriptErrorInvalidStackOperation,
  ScriptErrorOpReturn
}
import org.bitcoins.core.script.{
  ExecutedScriptProgram,
  ExecutionInProgressScriptProgram,
  StartedScriptProgram
}
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.util._
import org.bitcoins.testkit.util.{BitcoinSAsyncTest, TestUtil}

import scala.concurrent.Future

/**
  * Created by chris on 1/6/16.
  */
class ControlOperationsInterpreterTest extends BitcoinSAsyncTest {

  val COI: ControlOperationsInterpreter = ControlOperationsInterpreter
  "ControlOperationsInterpreter" must "have OP_VERIFY evaluate to true with '1' on the stack" in {
    val stack = List(OP_TRUE)
    val script = List(OP_VERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val result = COI.opVerify(program)
    result.stack.isEmpty must be(true)
    result.script.isEmpty must be(true)
  }

  it must "have OP_VERIFY evaluate to true when there are multiple items on the stack that can be cast to an int" in {
    //for this test case in bitcoin core's script test suite
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/script_valid.json#L21
    val stack = ScriptParser.fromString("0x09 0x00000000 0x00000000 0x10")
    val script = List(OP_VERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val result = COI.opVerify(program)
    result.stackTopIsTrue must be(true)
  }

  it must "have OP_VERIFY evaluate to false with '0' on the stack" in {
    val stack = List(OP_FALSE)
    val script = List(OP_VERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val result = COI.opVerify(program)
    result.stackTopIsFalse must be(true)
  }

  it must "mark the script as invalid for OP_VERIFY when there is nothing on the stack" in {

    val stack = List()
    val script = List(OP_VERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val result =
      ScriptProgramTestUtil.toExecutedScriptProgram(COI.opVerify(program))
    result.error must be(Some(ScriptErrorInvalidStackOperation))

  }

  it must "fail for verify when there is nothing on the script stack" in {
    recoverToSucceededIf[IllegalArgumentException] {
      Future {
        val stack = List(ScriptConstant("1"))
        val script = List()
        val program =
          TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                       script)
        COI.opVerify(program)
      }
    }
  }

  private def programMustBe(
      program: StartedScriptProgram,
      stack: List[ScriptToken],
      script: List[ScriptToken],
      shouldExecuteNextOperation: Boolean,
      isInExecutionBranch: Boolean): org.scalatest.Assertion = {
    program.stack must be(stack)
    program.script must be(script)

    program match {
      case programInProgress: ExecutionInProgressScriptProgram =>
        programInProgress.shouldExecuteNextOperation must be(
          shouldExecuteNextOperation)
        programInProgress.isInExecutionBranch must be(isInExecutionBranch)
      case executed: ExecutedScriptProgram =>
        fail(s"Unexpected error: ${executed.error}")
    }
  }

  it must "evaluate an OP_IF correctly" in {
    val stack = List(OP_0)
    val script = List(OP_IF, OP_RESERVED, OP_ENDIF, OP_1)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = COI.opIf(program)
    programMustBe(program = newProgram,
                  stack = Nil,
                  script = List(OP_RESERVED, OP_ENDIF, OP_1),
                  shouldExecuteNextOperation = false,
                  isInExecutionBranch = false)
    val newProgramInProgress =
      newProgram.asInstanceOf[ExecutionInProgressScriptProgram]

    val newNewProgram =
      newProgramInProgress.updateScript(newProgram.script.tail)
    programMustBe(program = newNewProgram,
                  stack = Nil,
                  script = List(OP_ENDIF, OP_1),
                  shouldExecuteNextOperation = true,
                  isInExecutionBranch = false)

    val afterIfProgram = COI.opEndIf(newNewProgram)
    programMustBe(program = afterIfProgram,
                  stack = Nil,
                  script = List(OP_1),
                  shouldExecuteNextOperation = true,
                  isInExecutionBranch = true)
  }

  it must "evaluate an OP_IF OP_ELSE OP_ENDIF block" in {
    val stack = List(OP_0)
    val script = List(OP_IF, OP_VER, OP_ELSE, OP_1, OP_ENDIF)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = COI.opIf(program)
    programMustBe(program = newProgram,
                  stack = Nil,
                  script = List(OP_VER, OP_ELSE, OP_1, OP_ENDIF),
                  shouldExecuteNextOperation = false,
                  isInExecutionBranch = false)
    val newProgramInProgress =
      newProgram.asInstanceOf[ExecutionInProgressScriptProgram]

    val elseProgram = newProgramInProgress.updateScript(newProgram.script.tail)
    programMustBe(program = elseProgram,
                  stack = Nil,
                  script = List(OP_ELSE, OP_1, OP_ENDIF),
                  shouldExecuteNextOperation = true,
                  isInExecutionBranch = false)

    val afterElseProgram = COI.opElse(elseProgram)
    programMustBe(program = afterElseProgram,
                  stack = Nil,
                  script = List(OP_1, OP_ENDIF),
                  shouldExecuteNextOperation = true,
                  isInExecutionBranch = true)
  }

  it must "evaluate an OP_IF block correctly if the stack top is true" in {
    val stack = List(ScriptNumber.one)
    val script = List(OP_IF, OP_1, OP_ELSE, OP_0, OP_ENDIF)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = COI.opIf(program)
    programMustBe(program = newProgram,
                  stack = Nil,
                  script = List(OP_1, OP_ELSE, OP_0, OP_ENDIF),
                  shouldExecuteNextOperation = true,
                  isInExecutionBranch = true)
    val newProgramInProgress =
      newProgram.asInstanceOf[ExecutionInProgressScriptProgram]

    val elseProgram =
      newProgramInProgress.updateStackAndScript(List(ScriptNumber.one),
                                                newProgram.script.tail)
    programMustBe(program = elseProgram,
                  stack = List(ScriptNumber.one),
                  script = List(OP_ELSE, OP_0, OP_ENDIF),
                  shouldExecuteNextOperation = true,
                  isInExecutionBranch = true)

    val afterElseProgram = COI.opElse(elseProgram)
    programMustBe(program = afterElseProgram,
                  stack = List(ScriptNumber.one),
                  script = List(OP_0, OP_ENDIF),
                  shouldExecuteNextOperation = false,
                  isInExecutionBranch = false)
  }

  it must "evaluate a weird case using multiple OP_ELSEs" in {
    val stack = List(ScriptNumber.one)
    val script = List(OP_IF, OP_ELSE, OP_0, OP_ELSE, OP_1, OP_ENDIF)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = COI.opIf(program)
    programMustBe(program = newProgram,
                  stack = Nil,
                  script = List(OP_ELSE, OP_0, OP_ELSE, OP_1, OP_ENDIF),
                  shouldExecuteNextOperation = true,
                  isInExecutionBranch = true)
    val newProgramInProgress =
      newProgram.asInstanceOf[ExecutionInProgressScriptProgram]

    val newNewProgram = COI.opElse(newProgramInProgress)
    programMustBe(program = newNewProgram,
                  stack = Nil,
                  script = List(OP_0, OP_ELSE, OP_1, OP_ENDIF),
                  shouldExecuteNextOperation = false,
                  isInExecutionBranch = false)
    val newNewProgramInProgress =
      newNewProgram.asInstanceOf[ExecutionInProgressScriptProgram]

    val elseProgram =
      newNewProgramInProgress.updateScript(newNewProgram.script.tail)
    programMustBe(program = elseProgram,
                  stack = Nil,
                  script = List(OP_ELSE, OP_1, OP_ENDIF),
                  shouldExecuteNextOperation = true,
                  isInExecutionBranch = false)

    val afterElseProgram = COI.opElse(elseProgram)
    programMustBe(program = afterElseProgram,
                  stack = Nil,
                  script = List(OP_1, OP_ENDIF),
                  shouldExecuteNextOperation = true,
                  isInExecutionBranch = true)
  }

  it must "evaluate nested OP_IFS correctly" in {
    val stack = List(OP_0, OP_1)
    val script = List(OP_IF,
                      OP_IF,
                      OP_0,
                      OP_ELSE,
                      OP_1,
                      OP_ENDIF,
                      OP_ELSE,
                      OP_IF,
                      OP_2,
                      OP_ELSE,
                      OP_3,
                      OP_ENDIF,
                      OP_ENDIF)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val secondIfProgram = COI.opIf(program)
    programMustBe(program = secondIfProgram,
                  stack = List(ScriptNumber.one),
                  script = script.tail,
                  shouldExecuteNextOperation = true,
                  isInExecutionBranch = false)
    val secondIfProgramInProgress =
      secondIfProgram.asInstanceOf[ExecutionInProgressScriptProgram]

    val ignoreProgram0 = COI.opIf(secondIfProgramInProgress)
    programMustBe(program = ignoreProgram0,
                  stack = List(ScriptNumber.one),
                  script = secondIfProgram.script.tail,
                  shouldExecuteNextOperation = false,
                  isInExecutionBranch = false)
    val ignoreProgram0InProgress =
      ignoreProgram0.asInstanceOf[ExecutionInProgressScriptProgram]

    val ignoreElseProgram =
      ignoreProgram0InProgress.updateScript(ignoreProgram0.script.tail)
    programMustBe(program = ignoreElseProgram,
                  stack = List(ScriptNumber.one),
                  script = ignoreProgram0.script.tail,
                  shouldExecuteNextOperation = true,
                  isInExecutionBranch = false)

    val ignoreProgram1 = COI.opElse(ignoreElseProgram)
    programMustBe(program = ignoreProgram1,
                  stack = List(ScriptNumber.one),
                  script = ignoreElseProgram.script.tail,
                  shouldExecuteNextOperation = false,
                  isInExecutionBranch = false)
    val ignoreProgram1InProgress =
      ignoreProgram1.asInstanceOf[ExecutionInProgressScriptProgram]

    val endIfProgram =
      ignoreProgram1InProgress.updateScript(ignoreProgram1.script.tail)
    programMustBe(program = endIfProgram,
                  stack = List(ScriptNumber.one),
                  script = ignoreProgram1.script.tail,
                  shouldExecuteNextOperation = true,
                  isInExecutionBranch = false)

    val elseProgram = COI.opEndIf(endIfProgram)
    programMustBe(program = elseProgram,
                  stack = List(ScriptNumber.one),
                  script = endIfProgram.script.tail,
                  shouldExecuteNextOperation = true,
                  isInExecutionBranch = false)
    val elseProgramInProgress =
      elseProgram.asInstanceOf[ExecutionInProgressScriptProgram]

    val correctIfProgram = COI.opElse(elseProgramInProgress)
    programMustBe(program = correctIfProgram,
                  stack = List(ScriptNumber.one),
                  script = elseProgram.script.tail,
                  shouldExecuteNextOperation = true,
                  isInExecutionBranch = true)
    val correctIfProgramInProgress =
      correctIfProgram.asInstanceOf[ExecutionInProgressScriptProgram]

    val correctBranchProgram = COI.opIf(correctIfProgramInProgress)
    programMustBe(program = correctBranchProgram,
                  stack = Nil,
                  script = correctIfProgram.script.tail,
                  shouldExecuteNextOperation = true,
                  isInExecutionBranch = true)
  }

  it must "evaluate an OP_ENDIF correctly" in {
    val stack = List(ScriptNumber(1), ScriptNumber(1))
    val script =
      List(OP_ENDIF, OP_ELSE, OP_RETURN, OP_ENDIF, OP_ADD, OP_2, OP_EQUAL)
    val program =
      TestUtil.testProgramExecutionInProgress
        .updateStackAndScript(stack, script)
        .addCondition(true)
    val newProgram = COI.opEndIf(program)

    newProgram.stack must be(stack)
    newProgram.script must be(script.tail)
  }

  it must "mark a transaction as invalid if it is trying to spend an OP_RETURN output" in {
    val stack = Seq()
    val script = Seq(OP_RETURN)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram =
      ScriptProgramTestUtil.toExecutedScriptProgram(COI.opReturn(program))

    newProgram.error must be(Some(ScriptErrorOpReturn))
  }
}
