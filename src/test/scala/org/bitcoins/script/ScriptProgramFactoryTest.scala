package org.bitcoins.script

import org.bitcoins.script.constant.{OP_1, OP_0}
import org.bitcoins.script.flag.ScriptFlagFactory
import org.bitcoins.util.TestUtil
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 3/31/16.
 */
class ScriptProgramFactoryTest extends FlatSpec with MustMatchers {

  "ScriptProgramFactory" must "update a field depending on its indicator" in {

    val stack = List(OP_0)
    val altStack = List(OP_1)

    val modifiedStackProgram = ScriptProgram(TestUtil.testProgram,stack,ScriptProgram.Stack)
    modifiedStackProgram.stack must be (stack)

    val modifiedAltStack = ScriptProgram(TestUtil.testProgram,altStack,ScriptProgram.AltStack)

    modifiedAltStack.altStack must be (altStack)
  }

  it must "update the OP_CODESEPARATOR index" in {
    val index = 999
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress,index)
    program.lastCodeSeparator must be (999)
  }

  it must "update the OP_CODESEPARATOR index & stack simultaneously" in {
    val index = 999
    val stack = Seq(OP_0)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress,stack,ScriptProgram.Stack,index)
    program.stack must be (stack)
    program.lastCodeSeparator must be (index)
  }

  it must "update the OP_CODESEPARATOR index & altStack simultaneously" in {
    val index = 999
    val altStack = Seq(OP_0)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress,altStack,ScriptProgram.AltStack,index)
    program.altStack must be (altStack)
    program.lastCodeSeparator must be (index)
  }


  it must "update the script program to the given stack and script" in {
    val stack = List(OP_0)
    val script = List(OP_1)
    val program = ScriptProgram(TestUtil.transaction, TestUtil.scriptPubKey, 0, stack,script, ScriptFlagFactory.empty)
    program.stack must be (stack)
    program.script must be (script)
  }
}
