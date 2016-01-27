package org.scalacoin.script.crypto

import org.scalacoin.protocol.script.ScriptPubKey
import org.scalacoin.script.constant.ScriptConstantImpl
import org.scalacoin.util.TestUtil
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/6/16.
 */
class CryptoInterpreterTest extends FlatSpec with MustMatchers with CryptoInterpreter {
  val stack = List(ScriptConstantImpl("02218AD6CDC632E7AE7D04472374311CEBBBBF0AB540D2D08C3400BB844C654231".toLowerCase))
  "CryptoInterpreter" must "evaluate OP_HASH160 correctly when it is on top of the script stack" in {

    val script = List(OP_HASH160)
    val (newStack,newScript) = hash160(stack,script)
    newStack.head must be (ScriptConstantImpl("5238C71458E464D9FF90299ABCA4A1D7B9CB76AB".toLowerCase))
    newScript.size must be (0)
  }

  it must "fail to evaluate OP_HASH160 when the stack is empty" in {
    intercept[IllegalArgumentException] {
      val stack = List()
      val script = List(OP_HASH160)
      val (newStack,newScript) = hash160(stack,script)
    }
  }

  it must "fail to evaluate OP_HASH160 when the script stack is empty" in {
    intercept[IllegalArgumentException] {
      val script = List()
      val (newStack,newScript) = hash160(stack,script)
    }
  }


  it must "evaluate a OP_CHECKSIG to true for a valid tx on the network" in {
    val tx = TestUtil.simpleTransaction
    val parentTx = TestUtil.parentSimpleTransaction
    val vout : Int = tx.inputs.head.previousOutput.vout
    require(vout == 0)
    val scriptPubKey = tx.outputs(vout).scriptPubKey
    val result = checkSig(tx,scriptPubKey)
    result must be (true)
  }

  it must "evaluate a OP_SHA1 correctly" in {
    val stack = List(ScriptConstantImpl("ab"))
    val script = List(OP_SHA1)
    val (newStack,newScript) = opSha1(stack,script)
    newStack.head must be (ScriptConstantImpl("fe83f217d464f6fdfa5b2b1f87fe3a1a47371196"))
    newScript.isEmpty must be (true)
  }
}
