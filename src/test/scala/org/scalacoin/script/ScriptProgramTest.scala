package org.scalacoin.script

import org.scalacoin.script.constant.{OP_0, ScriptFalse, ScriptNumberImpl}
import org.scalacoin.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/6/16.
 */
class ScriptProgramTest extends FlatSpec with MustMatchers  {

  "ScriptProgram" must "determine if the stack top is true" in {
    val stack = List(ScriptNumberImpl(1))
    val script = List()
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    program.stackTopIsTrue must be (true)
  }

  it must "determine if the stack stop is false" in {
    val stack = List(ScriptNumberImpl(0))
    val script = List()
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    program.stackTopIsTrue must be (false)


    val program1 = program.copy(stack = List(ScriptFalse))
    program1.stackTopIsTrue must be (false)

    val program2 = program.copy(stack = List(OP_0))
    program2.stackTopIsTrue must be (false)
  }


}
