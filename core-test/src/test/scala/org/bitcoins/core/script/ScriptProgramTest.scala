package org.bitcoins.core.script

import org.bitcoins.core.script.constant._
import org.bitcoins.testkitcore.util.TestUtil
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 2/6/16.
  */
class ScriptProgramTest extends BitcoinSUnitTest {

  "ScriptProgram" must "determine if the stack top is true" in {
    val stack = List(ScriptNumber(1))
    val script = List()
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    program.stackTopIsTrue must be(true)
  }

  it must "determine if the stack stop is false" in {
    val stack = List(ScriptNumber.zero)
    val script = List()
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    program.stackTopIsTrue must be(false)

    val program2 = program.updateStack(List(OP_0))
    program2.stackTopIsTrue must be(false)

    //stack top should not be true for negative zero
    val program3 = program.updateStack(List(ScriptNumber.negativeZero))
    program3.stackTopIsTrue must be(false)
  }
}
