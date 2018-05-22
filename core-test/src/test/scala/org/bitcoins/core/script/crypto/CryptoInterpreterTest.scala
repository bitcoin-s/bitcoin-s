package org.bitcoins.core.script.crypto

import org.bitcoins.core.crypto.BaseTxSigComponent
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{ P2SHScriptSignature, ScriptPubKey, ScriptSignature, SigVersionBase }
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script._
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.flag.{ ScriptFlagFactory, ScriptVerifyDerSig, ScriptVerifyNullDummy }
import org.bitcoins.core.script.result._
import org.bitcoins.core.util.{ BitcoinSLogger, ScriptProgramTestUtil, TestUtil, TransactionTestUtil }
import org.scalatest.{ FlatSpec, MustMatchers }

/**
 * Created by chris on 1/6/16.
 */
class CryptoInterpreterTest extends FlatSpec with MustMatchers {
  private def logger = BitcoinSLogger.logger
  val stack = List(ScriptConstant("02218AD6CDC632E7AE7D04472374311CEBBBBF0AB540D2D08C3400BB844C654231".toLowerCase))
  val CI = CryptoInterpreter
  "CryptoInterpreter" must "evaluate OP_HASH160 correctly when it is on top of the script stack" in {

    val script = List(OP_HASH160)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = CI.opHash160(program)

    newProgram.stack.head must be(ScriptConstant("5238C71458E464D9FF90299ABCA4A1D7B9CB76AB".toLowerCase))
    newProgram.script.size must be(0)
  }

  it must "mark the script as invalid when there are no arguments for OP_HASH160" in {
    val stack = List()
    val script = List(OP_HASH160)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val executedProgram: ExecutedScriptProgram = ScriptProgramTestUtil.toExecutedScriptProgram(CI.opHash160(program))
    executedProgram.error must be(Some(ScriptErrorInvalidStackOperation))

  }

  it must "fail to evaluate OP_HASH160 when the script stack is empty" in {
    intercept[IllegalArgumentException] {
      val script = List()
      val program = ScriptProgram(TestUtil.testProgram, stack, script)
      CI.opHash160(program)
    }
  }

  it must "evaluate an OP_RIPEMD160 correctly" in {
    val stack = List(ScriptConstant(""))
    val script = List(OP_RIPEMD160)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = CI.opRipeMd160(program)
    newProgram.stack must be(List(ScriptConstant("9c1185a5c5e9fc54612808977ee8f548b2258d31")))
    newProgram.script.isEmpty must be(true)
  }

  it must "evaluate a OP_SHA1 correctly" in {
    val stack = List(ScriptConstant("ab"))
    val script = List(OP_SHA1)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = CI.opSha1(program)
    newProgram.stack.head must be(ScriptConstant("fe83f217d464f6fdfa5b2b1f87fe3a1a47371196"))
    newProgram.script.isEmpty must be(true)
  }

  it must "evaluate an OP_SHA256 correctly" in {
    val stack = List(ScriptConstant(""))
    val script = List(OP_SHA256)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = CI.opSha256(program)
    newProgram.stack must be(List(ScriptConstant("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")))
    newProgram.script.isEmpty must be(true)
  }

  it must "evaluate an OP_HASH256 correctly" in {
    val stack = List(ScriptConstant(""))
    val script = List(OP_HASH256)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val newProgram = CI.opHash256(program)
    newProgram.stack must be(List(ScriptConstant("5df6e0e2761359d30a8275058e299fcc0381534545f55cf43e41983f5d4c9456")))
    newProgram.script.isEmpty must be(true)
  }

  it must "evaluate an OP_CHECKMULTISIG with zero signatures and zero pubkeys" in {
    val stack = List(OP_0, OP_0, OP_0)
    val script = List(OP_CHECKMULTISIG)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val programNoFlags = ScriptProgram(program, ScriptFlagFactory.empty)
    val newProgram = CI.opCheckMultiSig(programNoFlags)
    newProgram.stack must be(List(OP_TRUE))
    newProgram.script.isEmpty must be(true)
  }

  it must "evaluate an OP_CHECKMULTISIG and leave the remaining operations on the stack" in {
    val stack = List(OP_0, OP_0, OP_0, OP_16, OP_16, OP_16)
    val script = List(OP_CHECKMULTISIG, OP_16, OP_16, OP_16, OP_16)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val programNoFlags = ScriptProgram(program, ScriptFlagFactory.empty)
    val newProgram = CI.opCheckMultiSig(programNoFlags)
    newProgram.stack must be(List(OP_TRUE, OP_16, OP_16, OP_16))
    newProgram.script must be(List(OP_16, OP_16, OP_16, OP_16))
  }

  it must "evaluate an OP_CHECKMULTISIGVERIFY with zero signatures and zero pubkeys" in {
    val stack = List(ScriptNumber.zero, ScriptNumber.zero, ScriptNumber.zero)
    val script = List(OP_CHECKMULTISIGVERIFY)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val programNoFlags = ScriptProgram(program, ScriptFlagFactory.empty)
    val newProgram = CI.opCheckMultiSigVerify(programNoFlags)
    newProgram.script.isEmpty must be(true)
    newProgram.stack.isEmpty must be(true)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(false)
  }

  it must "evaluate an OP_CHECKMULTISIGVERIFY and leave the remaining operations on the stack" in {
    val stack = List(OP_0, OP_0, OP_0, OP_16, OP_16, OP_16)
    val script = List(OP_CHECKMULTISIGVERIFY, OP_16, OP_16, OP_16, OP_16)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val programNoFlags = ScriptProgram(program, ScriptFlagFactory.empty)
    val newProgram = CI.opCheckMultiSigVerify(programNoFlags)
    newProgram.stack must be(List(OP_16, OP_16, OP_16))
    newProgram.script must be(List(OP_16, OP_16, OP_16, OP_16))
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(false)
  }

  it must "evaluate an OP_CHECKMULTISIG for" in {
    //0 0 0 1 CHECKMULTISIG VERIFY DEPTH 0 EQUAL
    val stack = List(OP_1, OP_0, OP_0, OP_0)
    val script = List(OP_CHECKMULTISIG)
    val program = ScriptProgram(TestUtil.testProgram, stack, script)
    val programNoFlags = ScriptProgram(program, ScriptFlagFactory.empty)
    val newProgram = CI.opCheckMultiSig(programNoFlags)
    newProgram.stack must be(List(OP_TRUE))
    newProgram.script.isEmpty must be(true)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(false)
  }

  it must "mark a transaction invalid when the NULLDUMMY flag is set for a OP_CHECKMULTISIG operation & the scriptSig does not begin with OP_0" in {
    val flags = Seq(ScriptVerifyNullDummy)
    val scriptSig = ScriptSignature.fromAsm(Seq(OP_1))
    val input = TransactionInput(EmptyTransactionOutPoint, scriptSig, TransactionConstants.sequence)
    val empty = EmptyTransaction
    val tx = BaseTransaction(empty.version, Seq(input), empty.outputs, empty.lockTime)
    val t = BaseTxSigComponent(
      transaction = tx,
      inputIndex = UInt32.zero,
      output = TransactionOutput(CurrencyUnits.zero, TestUtil.scriptPubKey),
      flags = flags)
    val pre = PreExecutionScriptProgram(t)
    val baseProgram = ScriptProgram.toExecutionInProgress(pre)
    val stack = Seq(OP_0, OP_0, OP_1)
    val script = Seq(OP_CHECKMULTISIG)
    val program = ScriptProgram(baseProgram, stack, script)
    val executedProgram = CI.opCheckMultiSig(program)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(executedProgram)
    newProgram.error must be(Some(ScriptErrorSigNullDummy))

  }

  it must "mark a transaction invalid when the DERSIG flag is set for a OP_CHECKSIG operaetion & the signature is not a strict der sig" in {
    val flags = Seq(ScriptVerifyDerSig)
    //signature is from script_valid.json, it has a negative S value which makes it non strict der
    val stack = Seq(OP_0, ScriptConstant("302402107777777777777777777777777777777702108777777777777777777777777777777701"))
    val script = Seq(OP_CHECKSIG)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val programWithFlags = ScriptProgram(program, flags)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(CI.opCheckSig(programWithFlags))
    newProgram.error must be(Some(ScriptErrorSigDer))

  }

  it must "evaluate an OP_CODESEPARATOR" in {
    val stack = List()
    val script = Seq(OP_CODESEPARATOR)
    val program = ScriptProgram(ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script), script, ScriptProgram.OriginalScript)
    val newProgram = ScriptProgramTestUtil.toExecutionInProgressScriptProgram(CI.opCodeSeparator(program))
    newProgram.lastCodeSeparator must be(Some(0))
  }

}
