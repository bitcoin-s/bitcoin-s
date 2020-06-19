package org.bitcoins.core.script.bitwise

import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.result.ScriptErrorInvalidStackOperation
import org.bitcoins.core.script.ExecutedScriptProgram
import org.bitcoins.testkit.util.{BitcoinSUnitTest, TestUtil}

/**
  * Created by chris on 1/6/16.
  */
class BitwiseInterpreterTest extends BitcoinSUnitTest {

  private val pubKeyHash = ScriptConstant(
    "5238C71458E464D9FF90299ABCA4A1D7B9CB76AB".toLowerCase)
  val BI = BitwiseInterpreter
  "BitwiseInterpreter" must "evaluate OP_EQUAL" in {
    val stack = List(pubKeyHash, pubKeyHash)
    val script = List(OP_EQUAL)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = BI.opEqual(program)
    newProgram.stack.head must be(OP_TRUE)
  }

  it must "evaluate OP_1 and OP_TRUE to equal" in {
    val stack = List(OP_1, OP_TRUE)
    val script = List(OP_EQUAL)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = BI.opEqual(program)
    newProgram.stack.head must be(OP_TRUE)
  }

  it must "throw an exception for OP_EQUAL when we don't have enough items on the stack" in {
    intercept[IllegalArgumentException] {
      BI.opEqual(
        TestUtil.testProgramExecutionInProgress.updateStackAndScript(List(),
                                                                     List()))
    }
  }

  it must "throw an exception for OP_EQUAL when we don't have enough items on the script stack" in {
    intercept[IllegalArgumentException] {
      BI.opEqual(
        TestUtil.testProgramExecutionInProgress.updateStackAndScript(List(),
                                                                     List()))
    }
  }

  it must "evaulate OP_EQUALVERIFY must not evaluate a transaction to invalid with two of the same pubkeys" in {
    val stack = List(pubKeyHash, pubKeyHash)
    val script = List(OP_EQUALVERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val result = BI.opEqualVerify(program)
    //if verification fails it will transform the script to a ExecutedProgram with an error set
    result.isInstanceOf[ExecutedScriptProgram] must be(false)
  }

  it must "evaluate OP_EQUALVERIFY to false given two different pub keys" in {
    val uniquePubKey = ScriptConstant(pubKeyHash.hex + "00")
    val stack = List(pubKeyHash, uniquePubKey)
    val script = List(OP_EQUALVERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val result = BI.opEqualVerify(program)
    result.stackTopIsTrue must be(false)
  }

  it must "evaluate a ScriptNumber & ScriptConstant to true if they are the same" in {
    val stack = List(ScriptNumber(2), ScriptConstant("02"))
    val script = List(OP_EQUAL)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    BI.opEqual(program).stack.head must be(OP_TRUE)

    val stack1 = List(ScriptConstant("02"), ScriptNumber(2))
    val script1 = List(OP_EQUAL)
    val program1 =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack1,
                                                                   script1)
    BI.opEqual(program1).stack.head must be(OP_TRUE)
  }

  it must "evaluate an OP_0 and ScriptNumberImpl(0) to equal" in {
    val stack = List(OP_0, ScriptNumber.zero)
    val script = List(OP_EQUAL)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    BI.opEqual(program).stack.head must be(OP_TRUE)
  }

  it must "mark the script as invalid of OP_EQUALVERIFY is run without two stack elements" in {
    val stack = List(OP_0)
    val script = List(OP_EQUALVERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(stack,
                                                                   script)
    val newProgram = BI.opEqualVerify(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorInvalidStackOperation))
  }
}
