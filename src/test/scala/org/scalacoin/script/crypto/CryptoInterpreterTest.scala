package org.scalacoin.script.crypto

import org.scalacoin.protocol.script.{ScriptPubKeyFactory, ScriptSignatureFactory, ScriptPubKey}
import org.scalacoin.script.arithmetic.OP_NOT
import org.scalacoin.script.{ScriptProgramFactory, ScriptProgramImpl}
import org.scalacoin.script.constant._
import org.scalacoin.util.{TransactionTestUtil, BitcoinSLogger, TestUtil}
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

  it must "evaluate an OP_CHECKMULTISIG with zero signatures and zero pubkeys" in {
    val stack = List(OP_0,OP_0,OP_0)
    val script = List(OP_CHECKMULTISIG)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opCheckMultiSig(program)
    newProgram.isValid must be (false)
    newProgram.stack must be (List(ScriptFalse))
    newProgram.script.isEmpty must be (true)
  }

  it must "evaluate an OP_CHECKMULTISIG and leave the remaining operations on the stack" in {
    val stack = List(OP_0,OP_0,OP_0, OP_16,OP_16,OP_16)
    val script = List(OP_CHECKMULTISIG,OP_16,OP_16,OP_16,OP_16)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opCheckMultiSig(program)
    newProgram.stack must be (List(ScriptFalse, OP_16,OP_16,OP_16))
    newProgram.script must be (List(OP_16,OP_16,OP_16,OP_16))
  }

  it must "evaluate an OP_CHECKMULTISIGVERIFY with zero signatures and zero pubkeys" in {
    val stack = List(OP_0,OP_0,OP_0)
    val script = List(OP_CHECKMULTISIGVERIFY)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram, stack,script)
    val newProgram = opCheckMultiSigVerify(program)
    newProgram.isValid must be (false)
    newProgram.script.isEmpty must be (true)

    newProgram.stack.isEmpty must be (true)
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
    newProgram.stack must be (List(ScriptFalse))
    newProgram.script.isEmpty must be (true)
    newProgram.isValid must be (false)
  }

  it must "evaluate an OP_CHECKSIG for a p2pk transaction" in {
    val creditingTx = TransactionTestUtil.buildCreditingTransaction(TestUtil.p2pkScriptPubKey)
    val spendingTx = TransactionTestUtil.buildSpendingTransaction(creditingTx,TestUtil.p2pkScriptSig,0)
    val baseProgram = ScriptProgramFactory.factory(spendingTx,creditingTx.outputs(0).scriptPubKey,0,List())
    val stack = Seq(TestUtil.p2pkScriptPubKey.asm(1)) ++ TestUtil.p2pkScriptSig.asm.tail

    val script = List(TestUtil.p2pkScriptPubKey.asm.last)
    val program = ScriptProgramFactory.factory(baseProgram,stack,script)
    val newProgram = opCheckSig(program)
    newProgram.stack must be (List(ScriptTrue))
    newProgram.script.isEmpty must be (true)
  }


  it must "evaluate an OP_CHECKMULTISIG for a p2sh transaction" in {

    val rawScriptSig = "0047304402205b7d2c2f177ae76cfbbf14d589c113b0b35db753d305d5562dd0b61cbf366cfb02202e56f93c4f08a27f986cd424ffc48a462c3202c4902104d4d0ff98ed28f4bf80014730440220563e5b3b1fc11662a84bc5ea2a32cc3819703254060ba30d639a1aaf2d5068ad0220601c1f47ddc76d93284dd9ed68f7c9974c4a0ea7cbe8a247d6bc3878567a5fca014c6952210279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f8179821038282263212c609d9ea2a6e3e172de238d8c39cabd5ac1ca10646e23fd5f515082103363d90d447b00c9c99ceac05b6262ee053441c7e55552ffe526bad8f83ff464053ae"
    val p2shScriptSig = ScriptSignatureFactory.fromHex(rawScriptSig)

    val rawScriptPubKey = "a914c9e4a896d149702d0d1695434feddd52e24ad78d87"
    val p2shScriptPubKey = ScriptPubKeyFactory.fromHex(rawScriptPubKey)


    val creditingTx = TransactionTestUtil.buildCreditingTransaction(p2shScriptPubKey)
    val spendingTx = TransactionTestUtil.buildSpendingTransaction(creditingTx,p2shScriptSig,0)

    val stack = List(ScriptNumberImpl(3),
      ScriptConstantImpl("03363d90d447b00c9c99ceac05b6262ee053441c7e55552ffe526bad8f83ff4640"),
      ScriptConstantImpl("038282263212c609d9ea2a6e3e172de238d8c39cabd5ac1ca10646e23fd5f51508"),
      ScriptConstantImpl("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
      ScriptNumberImpl(2),
      ScriptConstantImpl("30440220563e5b3b1fc11662a84bc5ea2a32cc3819703254060ba30d639a1aaf2d5068ad0220601c1f47ddc76d93284dd9ed68f7c9974c4a0ea7cbe8a247d6bc3878567a5fca01"),
      ScriptConstantImpl("304402205b7d2c2f177ae76cfbbf14d589c113b0b35db753d305d5562dd0b61cbf366cfb02202e56f93c4f08a27f986cd424ffc48a462c3202c4902104d4d0ff98ed28f4bf8001"),
      OP_0)

    val script = List(OP_CHECKMULTISIG)

    val baseProgram = ScriptProgramFactory.factory(spendingTx,creditingTx.outputs(0).scriptPubKey,0,List())

    val program = ScriptProgramFactory.factory(baseProgram,stack,script)
    val newProgram = opCheckMultiSig(program)

    newProgram.stackTopIsTrue must be (true)
    newProgram.stack.size must be (1)

    newProgram.script.isEmpty must be (true)


  }


}
