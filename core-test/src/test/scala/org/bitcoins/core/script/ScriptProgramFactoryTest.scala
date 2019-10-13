package org.bitcoins.core.script

import org.bitcoins.core.crypto.BaseTxSigComponent
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.script.constant.{OP_0, OP_1}
import org.bitcoins.core.script.flag.ScriptFlagFactory
import org.bitcoins.core.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 3/31/16.
  */
class ScriptProgramFactoryTest extends FlatSpec with MustMatchers {

  "ScriptProgramFactory" must "update a field depending on its indicator" in {

    val stack = List(OP_0)
    val altStack = List(OP_1)

    val modifiedStackProgram =
      TestUtil.testProgramExecutionInProgress.updateStack(stack)
    modifiedStackProgram.stack must be(stack)

    val modifiedAltStack =
      TestUtil.testProgramExecutionInProgress.updateAltStack(altStack)

    modifiedAltStack.altStack must be(altStack)
  }

  it must "update the OP_CODESEPARATOR index" in {
    val index = 999
    val program =
      TestUtil.testProgramExecutionInProgress.updateLastCodeSeparator(index)
    program.lastCodeSeparator must be(Some(index))
  }

  it must "update the OP_CODESEPARATOR index & stack simultaneously" in {
    val index = 999
    val stack = Seq(OP_0)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress,
                                stack,
                                ScriptProgram.Stack,
                                index)
    program.stack must be(stack)
    program.lastCodeSeparator must be(Some(index))
  }

  it must "update the OP_CODESEPARATOR index & altStack simultaneously" in {
    val index = 999
    val altStack = Seq(OP_0)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress,
                                altStack,
                                ScriptProgram.AltStack,
                                index)
    program.altStack must be(altStack)
    program.lastCodeSeparator must be(Some(index))
  }

  it must "update the script program to the given stack and script" in {
    val stack = List(OP_0)
    val script = List(OP_1)
    val t = BaseTxSigComponent(
      transaction = TestUtil.transaction,
      inputIndex = UInt32.zero,
      output = TransactionOutput(CurrencyUnits.zero, TestUtil.scriptPubKey),
      ScriptFlagFactory.empty)
    val program = PreExecutionScriptProgram(t)
    val inProgress =
      ExecutionInProgressScriptProgram(t, stack, script, Nil, Nil, Nil, None)
    inProgress.stack must be(stack)
    inProgress.script must be(script)
  }
}
