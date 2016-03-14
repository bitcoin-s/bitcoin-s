package org.scalacoin.script.crypto

import org.scalacoin.protocol.script.ScriptPubKey
import org.scalacoin.script.{ScriptProgramFactory, ScriptProgramImpl}
import org.scalacoin.script.constant._
import org.scalacoin.util.{BitcoinSLogger, TestUtil}
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/6/16.
 */
class CryptoInterpreterTest extends FlatSpec with MustMatchers with CryptoInterpreter with BitcoinSLogger {
  val stack = List(ScriptConstantImpl("02218AD6CDC632E7AE7D04472374311CEBBBBF0AB540D2D08C3400BB844C654231".toLowerCase))
  "CryptoInterpreter" must "evaluate OP_HASH160 correctly when it is on top of the script stack" in {

    val script = List(OP_HASH160)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opHash160(program)

    newProgram.stack.head must be (ScriptConstantImpl("5238C71458E464D9FF90299ABCA4A1D7B9CB76AB".toLowerCase))
    newProgram.script.size must be (0)
  }

  it must "fail to evaluate OP_HASH160 when the stack is empty" in {
    intercept[IllegalArgumentException] {
      val stack = List()
      val script = List(OP_HASH160)
      val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
      opHash160(program)
    }
  }

  it must "fail to evaluate OP_HASH160 when the script stack is empty" in {
    intercept[IllegalArgumentException] {
      val script = List()
      val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
      opHash160(program)
    }
  }

  it must "evaluate an OP_RIPEMD160 correctly" in {
    val stack = List(ScriptConstantImpl(""))
    val script = List(OP_RIPEMD160)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opRipeMd160(program)
    newProgram.stack must be (List(ScriptConstantImpl("9c1185a5c5e9fc54612808977ee8f548b2258d31")))
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate a OP_SHA1 correctly" in {
    val stack = List(ScriptConstantImpl("ab"))
    val script = List(OP_SHA1)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opSha1(program)
    newProgram.stack.head must be (ScriptConstantImpl("fe83f217d464f6fdfa5b2b1f87fe3a1a47371196"))
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_SHA256 correctly" in {
    val stack = List(ScriptConstantImpl(""))
    val script = List(OP_SHA256)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opSha256(program)
    newProgram.stack must be (List(ScriptConstantImpl("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")))
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_HASH256 correctly" in {
    val stack = List(ScriptConstantImpl(""))
    val script = List(OP_HASH256)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opHash256(program)
    newProgram.stack must be (List(ScriptConstantImpl("5df6e0e2761359d30a8275058e299fcc0381534545f55cf43e41983f5d4c9456")))
    newProgram.script.isEmpty must be (true)
  }


  it must "evaluate a OP_CHECKSIG to true for a valid tx on the network" in {
/*    val tx = TestUtil.simpleTransaction
    val parentTx = TestUtil.parentSimpleTransaction
    val vout : Int = tx.inputs.head.previousOutput.vout
    require(vout == 0)
    val scriptPubKey = tx.outputs(vout).scriptPubKey
    val result = checkSig(tx,scriptPubKey)
    result must be (true)*/
  }



  it must "evaluate an OP_CHECKMULTISIG with zero signatures and zero pubkeys" in {
    val stack = List(OP_0,OP_0,OP_0)
    val script = List(OP_CHECKMULTISIG)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opCheckMultiSig(program)
    newProgram.valid must be (true)
    newProgram.stack must be (List(ScriptTrue))
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_CHECKMULTISIG and leave the remaining operations on the stack" in {
    val stack = List(OP_0,OP_0,OP_0, OP_16,OP_16,OP_16)
    val script = List(OP_CHECKMULTISIG,OP_16,OP_16,OP_16,OP_16)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opCheckMultiSig(program)
    newProgram.stack must be (List(ScriptTrue, OP_16,OP_16,OP_16))
    newProgram.script must be (List(OP_16,OP_16,OP_16,OP_16))
  }

  it must "evaluate an OP_CHECKMULTISIGVERIFY with zero signatures and zero pubkeys" in {
    val stack = List(OP_0,OP_0,OP_0)
    val script = List(OP_CHECKMULTISIGVERIFY)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opCheckMultiSigVerify(program)
    newProgram.valid must be (true)
    newProgram.script.isEmpty must be (true)
    newProgram.stack.isEmpty must be (true)
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_CHECKMULTISIGVERIFY and leave the remaining operations on the stack" in {
    val stack = List(OP_0,OP_0,OP_0, OP_16,OP_16,OP_16)
    val script = List(OP_CHECKMULTISIGVERIFY,OP_16,OP_16,OP_16,OP_16)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opCheckMultiSigVerify(program)
    newProgram.stack must be (List(OP_16,OP_16,OP_16))
    newProgram.script must be (List(OP_16,OP_16,OP_16,OP_16))
  }

  it must "evaluate an OP_CHECKMULTISIG for" in {
    //0 0 0 1 CHECKMULTISIG VERIFY DEPTH 0 EQUAL
    val stack = List(OP_1,OP_0,OP_0,OP_0)
    val script = List(OP_CHECKMULTISIG)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opCheckMultiSig(program)
    newProgram.stack must be (List(ScriptTrue))
    newProgram.script.isEmpty must be (true)
    newProgram.valid must be (true)

  }

}
