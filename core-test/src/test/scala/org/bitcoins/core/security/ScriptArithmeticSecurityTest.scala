package org.bitcoins.core.security

import org.bitcoins.core.crypto.{BaseTxSigComponent, TxSigComponent}
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.script.arithmetic.{ArithmeticInterpreter, OP_1ADD}
import org.bitcoins.core.script.constant.{ScriptConstant, ScriptNumber}
import org.bitcoins.core.script.flag.{ScriptFlag, ScriptVerifyNone}
import org.bitcoins.core.script.locktime.{
  LockTimeInterpreter,
  OP_CHECKSEQUENCEVERIFY
}
import org.bitcoins.core.script.result.ScriptErrorNegativeLockTime
import org.bitcoins.core.script.{
  ExecutedScriptProgram,
  PreExecutionScriptProgram
}
import org.bitcoins.testkitcore.util.{BitcoinSUnitTest, TestUtil}

/** Security reproduction tests for script number / opcode semantics. Every test
  * in this file asserts the CORRECT (Bitcoin Core compatible) behavior, so each
  * test FAILS until the underlying bug is fixed.
  */
class ScriptArithmeticSecurityTest extends BitcoinSUnitTest {

  "ScriptArithmeticSecurity" must "fail OP_CHECKSEQUENCEVERIFY for ANY negative operand with ScriptErrorNegativeLockTime" in {
    // Finding 1 (High): core/src/main/scala/org/bitcoins/core/script/locktime/LockTimeInterpreter.scala:92-113
    // only rejects ScriptNumber.negativeOne; other negatives (e.g. -2) have the BIP68 disable
    // bit set in two's complement and are treated as a NOP (BIP112 bypass).
    // Correct behavior per BIP112: any operand < 0 fails with SCRIPT_ERR_NEGATIVE_LOCKTIME.
    val stack = List(ScriptNumber(-2))
    val script = List(OP_CHECKSEQUENCEVERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(
        stack,
        script
      )
    val newProgram = LockTimeInterpreter.opCheckSequenceVerify(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorNegativeLockTime)
    )
  }

  it must "fail a 5-byte numeric operand to OP_1ADD like Bitcoin Core's 4-byte CScriptNum limit" in {
    // Finding 2 (Medium): core/src/main/scala/org/bitcoins/core/script/arithmetic/ArithmeticInterpreter.scala:306
    // re-encodes ScriptConstant operands via ScriptNumberUtil.toLong, shrinking a non-minimal
    // 5-byte encoding of a small number to <= 4 bytes, so the consensus CScriptNum size check
    // (isLargerThan4Bytes) is bypassed when SCRIPT_VERIFY_MINIMALDATA is not set.
    // Correct behavior: Core fails any numeric operand larger than 4 bytes.
    val stack =
      List(ScriptConstant("ff00000000")) // 5-byte non-minimal encoding of 255
    val script = List(OP_1ADD)
    val t = buildTxSigComponent(Seq(ScriptVerifyNone))
    val program = PreExecutionScriptProgram(t).toExecutionInProgress
      .updateStackAndScript(stack, script)
    val newProgram = ArithmeticInterpreter.op1Add(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error.isDefined must be(
      true
    )
  }

  private def buildTxSigComponent(flags: Seq[ScriptFlag]): TxSigComponent = {
    BaseTxSigComponent(
      transaction = TestUtil.transaction,
      inputIndex = TestUtil.testProgram.txSignatureComponent.inputIndex,
      output = TransactionOutput(
        CurrencyUnits.zero,
        TestUtil.testProgram.txSignatureComponent.scriptPubKey
      ),
      flags = flags
    )
  }
}
