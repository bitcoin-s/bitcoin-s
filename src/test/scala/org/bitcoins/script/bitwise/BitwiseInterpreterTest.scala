package org.bitcoins.script.bitwise

import org.bitcoins.script.{ExecutedScriptProgram, ScriptProgram}
import org.bitcoins.script.arithmetic.OP_NUMEQUAL
import org.bitcoins.script.constant._
import org.bitcoins.util.TestUtil
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/6/16.
 */
class BitwiseInterpreterTest extends FlatSpec with MustMatchers with BitwiseInterpreter {
  private val pubKeyHash = ScriptConstantImpl("5238C71458E464D9FF90299ABCA4A1D7B9CB76AB".toLowerCase)

  "BitwiseInterpreter" must "evaluate OP_EQUAL" in {
    val stack = List(pubKeyHash, pubKeyHash)
    val script = List(OP_EQUAL)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    val newProgram = opEqual(program)
    newProgram.stack.head must be (OP_TRUE)
  }


  it must "evaluate OP_1 and OP_TRUE to equal" in {
    val stack = List(OP_1, OP_TRUE)
    val script = List(OP_EQUAL)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = opEqual(program)
    newProgram.stack.head must be (OP_TRUE)
  }

  it must "throw an exception for OP_EQUAL when we don't have enough items on the stack" in {
    intercept[IllegalArgumentException] {
      opEqual(ScriptProgram(TestUtil.testProgram, List(),List()))
    }
  }


  it must "throw an exception for OP_EQUAL when we don't have enough items on the script stack" in {
    intercept[IllegalArgumentException] {
      opEqual(ScriptProgram(TestUtil.testProgram, List(),List()))
    }
  }

  it must "evaulate OP_EQUALVERIFY must not evaluate a transaction to invalid with two of the same pubkeys" in {
    val stack = List(pubKeyHash, pubKeyHash)
    val script = List(OP_EQUALVERIFY)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    val result = opEqualVerify(program)
    //if verification fails it will transform the script to a ExecutedProgram with an error set
    result.isInstanceOf[ExecutedScriptProgram] must be (false)
  }

  it must "evaluate OP_EQUALVERIFY to false given two different pub keys" in {
    val uniquePubKey = ScriptConstantImpl(pubKeyHash.hex +"00")
    val stack = List(pubKeyHash,uniquePubKey)
    val script = List(OP_EQUALVERIFY)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack,script)
    val result = opEqualVerify(program)
    result.stackTopIsTrue must be (false)
  }


  it must "evaluate a ScriptNumber & ScriptConstant to true if they are the same" in {
    val stack = List(ScriptNumber(2), ScriptConstantImpl("02"))
    val script = List(OP_EQUAL)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    opEqual(program).stack.head must be (OP_TRUE)

    val stack1 = List( ScriptConstantImpl("02"),ScriptNumber(2))
    val script1 = List(OP_EQUAL)
    val program1 = ScriptProgram(TestUtil.testProgram, stack1,script1)
    opEqual(program1).stack.head must be (OP_TRUE)
  }

  it must "evaluate an OP_0 and ScriptNumberImpl(0) to equal" in {
    val stack = List(OP_0, ScriptNumber.zero)
    val script = List(OP_EQUAL)
    val program = ScriptProgram(TestUtil.testProgram, stack,script)
    opEqual(program).stack.head must be (OP_TRUE)
  }
}
