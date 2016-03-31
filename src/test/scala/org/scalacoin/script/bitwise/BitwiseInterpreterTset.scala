package org.scalacoin.script.bitwise

import org.scalacoin.script.{ScriptProgramFactory, ScriptProgramImpl}
import org.scalacoin.script.arithmetic.OP_NUMEQUAL
import org.scalacoin.script.constant._
import org.scalacoin.util.TestUtil
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/6/16.
 */
class BitwiseInterpreterTest extends FlatSpec with MustMatchers with BitwiseInterpreter {
  private val pubKeyHash = ScriptConstantImpl("5238C71458E464D9FF90299ABCA4A1D7B9CB76AB".toLowerCase)

  "BitwiseInterpreter" must "evaluate OP_EQUAL" in {
    val stack = List(pubKeyHash, pubKeyHash)
    val script = List(OP_EQUAL)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opEqual(program)
    newProgram.stack.head must be (ScriptTrue)
  }

  it must "throw an exception for OP_EQUAL when we don't have enough items on the stack" in {
    intercept[IllegalArgumentException] {
      opEqual(ScriptProgramFactory.factory(TestUtil.testProgram, List(),List()))
    }
  }


  it must "throw an exception for OP_EQUAL when we don't have enough items on the script stack" in {
    intercept[IllegalArgumentException] {
      opEqual(ScriptProgramFactory.factory(TestUtil.testProgram, List(),List()))
    }
  }

  it must "evaulate OP_EQUALVERIFY to true given two of the same pub keys" in {
    val stack = List(pubKeyHash, pubKeyHash)
    val script = List(OP_EQUALVERIFY)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val result = opEqualVerify(program)
    result.isValid must be (true)
  }

  it must "evaluate OP_EQUALVERIFY to false given two different pub keys" in {
    val uniquePubKey = ScriptConstantImpl(pubKeyHash +"0")
    val stack = List(pubKeyHash,uniquePubKey)
    val script = List(OP_EQUALVERIFY)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val result = opEqualVerify(program)
    result.isValid must be (false)
  }


  it must "evaluate a ScriptNumber & ScriptConstant to true if they are the same" in {
    val stack = List(ScriptNumberImpl(2), ScriptConstantImpl("02"))
    val script = List(OP_EQUAL)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    opEqual(program).stack.head must be (ScriptTrue)

    val stack1 = List( ScriptConstantImpl("02"),ScriptNumberImpl(2))
    val script1 = List(OP_EQUAL)
    val program1 = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    opEqual(program1).stack.head must be (ScriptTrue)
  }

  it must "evaluate an OP_0 and ScriptNumberImpl(0) to equal" in {
    val stack = List(OP_0, ScriptNumberImpl(0))
    val script = List(OP_EQUAL)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    opEqual(program).stack.head must be (ScriptTrue)
  }


  it must "evaluate an OP_1 and ScriptNumberImpl(1) to equal" in {
    val stack = List(OP_1, ScriptNumberImpl(1))
    val script = List(OP_EQUAL)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    opEqual(program).stack.head must be (ScriptTrue)
  }

  it must "evaluate two "




}
