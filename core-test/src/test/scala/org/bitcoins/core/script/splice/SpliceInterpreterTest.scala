package org.bitcoins.core.script.splice

import org.bitcoins.core.script.ExecutedScriptProgram
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.result.ScriptErrorInvalidStackOperation
import org.bitcoins.testkitcore.util.TestUtil
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 2/4/16.
  */
class SpliceInterpreterTest extends BitcoinSUnitTest {
  val SI = SpliceInterpreter

  "SpliceInterpreter" must "evaluate an OP_SIZE on OP_0 correctly" in {
    val stack = List(OP_0)
    val script = List(OP_SIZE)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = SI.opSize(program)
    newProgram.stack must be(List(OP_0, OP_0))
    newProgram.script.isEmpty must be(true)

  }

  it must "deterine the size of script number 0 correctly" in {
    val stack = List(ScriptNumber.zero)
    val script = List(OP_SIZE)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = SI.opSize(program)
    newProgram.stack must be(List(ScriptNumber.zero, ScriptNumber.zero))
    newProgram.script.isEmpty must be(true)
  }

  it must "evaluate an OP_SIZE correctly with 0x7f" in {
    val stack = List(ScriptConstant("7f"))
    val script = List(OP_SIZE)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = SI.opSize(program)
    newProgram.stack must be(List(ScriptNumber(1), ScriptConstant("7f")))
    newProgram.script.isEmpty must be(true)
  }

  it must "evaluate an OP_SIZE correctly with 0x8000" in {
    //0x8000 == 128 in bitcoin
    val stack = List(ScriptNumber(128))
    val script = List(OP_SIZE)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = SI.opSize(program)
    newProgram.stack must be(List(ScriptNumber(2), ScriptNumber(128)))
    newProgram.script.isEmpty must be(true)
  }

  it must "evaluate an OP_SIZE correctly with a negative number" in {
    val stack = List(ScriptNumber(-1))
    val script = List(OP_SIZE)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = SI.opSize(program)
    newProgram.stack must be(List(ScriptNumber.one, ScriptNumber(-1)))
    newProgram.script.isEmpty must be(true)
  }

  it must "mark the script as invalid if OP_SIZE has nothing on the stack" in {
    val stack = List()
    val script = List(OP_SIZE)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = SI.opSize(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorInvalidStackOperation))
  }
}
