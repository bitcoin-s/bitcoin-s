package org.scalacoin.script.bitwise

import org.scalacoin.script.constant.ScriptConstantImpl
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/6/16.
 */
class BitwiseInterpreterTest extends FlatSpec with MustMatchers with BitwiseInterpreter {
  private val pubKeyHash = ScriptConstantImpl("5238C71458E464D9FF90299ABCA4A1D7B9CB76AB".toLowerCase)

  "BitwiseInterpreter" must "evaluate OP_EQUAL" in {
    val stack = List(pubKeyHash, pubKeyHash)
    val script = List(OP_EQUAL)

    val (newStack,newScript) = equal(stack,script)
    newStack.head must be (ScriptConstantImpl("1"))
  }

  it must "throw an exception for OP_EQUAL when we don't have enough items on the stack" in {
    intercept[IllegalArgumentException] {
      equal(List(), List(OP_EQUAL))
    }
  }

  it must "throw an exception for OP_EQUAL when we don't have enough items on the script stack" in {
    intercept[IllegalArgumentException] {
      equal(List(pubKeyHash), List())
    }
  }

  it must "evaulate OP_EQUALVERIFY to true given two of the same pub keys" in {
    val stack = List(pubKeyHash, pubKeyHash)
    val script = List(OP_EQUALVERIFY)
    val result = equalVerify(stack,script)
    result must be (true)
  }

  it must "evaluate OP_EQUALVERIFY to false given two different pub keys" in {
    val uniquePubKey = ScriptConstantImpl(pubKeyHash +"0")
    val stack = List(pubKeyHash,uniquePubKey)
    val script = List(OP_EQUALVERIFY)
    val result = equalVerify(stack,script)
    result must be (false)
  }

  it must "throw an exception for OP_EQUALVERIFY when we don't have enough args in stack" in {
    intercept[IllegalArgumentException] {
      equalVerify(List(),List(OP_EQUALVERIFY))
    }
  }

  it must "throw an exception for OP_EQUALVERIFY when we don't have enough args in script stack" in {
    intercept[IllegalArgumentException] {
      equalVerify(List(pubKeyHash),List())
    }
  }

}
