package org.bitcoins.core.security

import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.locktime.{
  LockTimeInterpreter,
  OP_CHECKSEQUENCEVERIFY
}
import org.bitcoins.core.script.result.ScriptErrorNegativeLockTime
import org.bitcoins.core.script.ExecutedScriptProgram
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
}
