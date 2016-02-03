package org.scalacoin.script.bitwise

import org.scalacoin.script.ScriptProgramImpl
import org.scalacoin.script.constant.{ScriptTrue, ScriptConstantImpl}
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
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction)
    val newProgram = equal(program)
    newProgram.stack.head must be (ScriptTrue)
  }

  it must "throw an exception for OP_EQUAL when we don't have enough items on the stack" in {
    intercept[IllegalArgumentException] {
      equal(ScriptProgramImpl(List(), List(OP_EQUAL),TestUtil.transaction))
    }
  }


  it must "throw an exception for OP_EQUAL when we don't have enough items on the script stack" in {
    intercept[IllegalArgumentException] {
      equal(ScriptProgramImpl(List(pubKeyHash), List(),TestUtil.transaction))
    }
  }

  it must "evaulate OP_EQUALVERIFY to true given two of the same pub keys" in {
    val stack = List(pubKeyHash, pubKeyHash)
    val script = List(OP_EQUALVERIFY)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction)
    val result = equalVerify(program)
    result.valid must be (true)
  }

  it must "evaluate OP_EQUALVERIFY to false given two different pub keys" in {
    val uniquePubKey = ScriptConstantImpl(pubKeyHash +"0")
    val stack = List(pubKeyHash,uniquePubKey)
    val script = List(OP_EQUALVERIFY)
    val program = ScriptProgramImpl(stack,script,TestUtil.transaction)
    val result = equalVerify(program)
    result.valid must be (false)
  }

  it must "throw an exception for OP_EQUALVERIFY when we don't have enough args in stack" in {
    intercept[IllegalArgumentException] {
      equalVerify(ScriptProgramImpl(List(),List(OP_EQUALVERIFY),TestUtil.transaction))
    }
  }

  it must "throw an exception for OP_EQUALVERIFY when we don't have enough args in script stack" in {
    intercept[IllegalArgumentException] {
      equalVerify(ScriptProgramImpl(List(pubKeyHash),List(),TestUtil.transaction))
    }
  }

}
