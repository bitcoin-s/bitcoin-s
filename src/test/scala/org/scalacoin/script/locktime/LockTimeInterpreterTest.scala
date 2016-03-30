package org.scalacoin.script.locktime

import org.scalacoin.script.ScriptProgramFactory
import org.scalacoin.script.constant.OP_0
import org.scalacoin.util.TestUtil
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 3/30/16.
 */
class LockTimeInterpreterTest extends FlatSpec with MustMatchers with LockTimeInterpreter {

  "LockTimeInterpreter" must "mark the transaction invalid if the stack is empty" in {
    val stack = Seq()
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram,stack,script)
    val newProgram = opCheckLockTimeVerify(program)
    newProgram.isValid must be (false)
  }

  it must "mark the transaction invalid if the transaction's sequence number is set to the max" in {
    val stack = Seq(OP_0)
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram,stack,script)
    val newProgram = opCheckLockTimeVerify(program)
    newProgram.isValid must be (false)
  }
}
