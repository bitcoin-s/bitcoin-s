package org.bitcoins.core.security

import org.bitcoins.core.crypto.{BaseTxSigComponent, TxSigComponent}
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  EmptyTransaction,
  TransactionInput,
  TransactionOutput
}
import org.bitcoins.core.script.arithmetic.{ArithmeticInterpreter, OP_1ADD}
import org.bitcoins.core.script.constant.{
  BytesToPushOntoStack,
  OP_16,
  ScriptConstant,
  ScriptNumber
}
import org.bitcoins.core.script.flag.{ScriptFlag, ScriptVerifyNone}
import org.bitcoins.core.script.locktime.{
  LockTimeInterpreter,
  OP_CHECKLOCKTIMEVERIFY,
  OP_CHECKSEQUENCEVERIFY
}
import org.bitcoins.core.script.result.ScriptErrorNegativeLockTime
import org.bitcoins.core.script.stack.{OP_IFDUP, OP_ROLL, StackInterpreter}
import org.bitcoins.core.script.{
  ExecutedScriptProgram,
  PreExecutionScriptProgram
}
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.util.BitcoinScriptUtil
import org.bitcoins.testkitcore.util.{BitcoinSUnitTest, TestUtil}
import scodec.bits.ByteVector

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

  it must "fail gracefully with a ScriptError on numeric operands larger than 8 bytes instead of throwing NumberFormatException" in {
    // Finding 3 (High): core/src/main/scala/org/bitcoins/core/script/constant/ScriptNumberUtil.scala:108
    // parses operand hex with java.lang.Long.parseLong; a >8-byte operand overflows and throws
    // NumberFormatException, which escapes the interpreter (ArithmeticInterpreter.scala:306;
    // same pattern at ArithmeticInterpreter.scala:357-368 and LockTimeInterpreter.scala:67,116).
    // Correct behavior: a script evaluation error, not a crash.
    val stack = List(ScriptConstant("ffffffffffffffff7f")) // 9-byte operand
    val script = List(OP_1ADD)
    val t = buildTxSigComponent(Seq(ScriptVerifyNone))
    val program = PreExecutionScriptProgram(t).toExecutionInProgress
      .updateStackAndScript(stack, script)
    // currently throws NumberFormatException out of the interpreter
    val newProgram = ArithmeticInterpreter.op1Add(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error.isDefined must be(
      true
    )
  }

  it must "parse opcode bytes 0xbb-0xff as OP_SUCCESS187-254 operations" in {
    // Finding 4 (High): core/src/main/scala/org/bitcoins/core/script/ScriptOperationFactory.scala:64-66
    // and core/src/main/scala/org/bitcoins/core/serializers/script/ScriptParser.scala:175 —
    // fromByte does a Map lookup with no fallback for unassigned opcode bytes; per BIP342
    // bytes 187-254 are OP_SUCCESS187-254 and must be parseable/usable.
    // NOTE: in this snapshot these bytes do NOT throw NoSuchElementException; they parse to
    // ReservedOperation.UndefinedOP_NOP (ReservedOperations.scala:97-100), which is still
    // wrong per BIP342 — they are OP_SUCCESSx operations, not reserved NOPs.
    (187 to 254).foreach { opCode =>
      val parsed = ScriptParser.fromBytes(ByteVector(opCode.toByte))
      parsed.size must be(1)
      parsed.head.toString.startsWith("OP_SUCCESS") must be(true)
    }
  }

  it must "fail OP_CHECKLOCKTIMEVERIFY with a non-minimally encoded locktime when the minimal data flag is set" in {
    // Finding 5 (Low): core/src/main/scala/org/bitcoins/core/script/locktime/LockTimeInterpreter.scala:33-71
    // opCheckLockTimeVerify never checks ScriptFlagUtil.requireMinimalData / isShortestEncoding
    // (unlike opCheckSequenceVerify at LockTimeInterpreter.scala:95-97).
    // Correct behavior: a non-minimal locktime operand fails when SCRIPT_VERIFY_MINIMALDATA is set
    // (Core's CScriptNum enforces fRequireMinimal for CLTV too).
    val stack = Seq(ScriptNumber("0100")) // non-minimal encoding of 1
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val oldInput = TestUtil.transaction.inputs.head
    val input = TransactionInput(
      oldInput.previousOutput,
      oldInput.scriptSignature,
      UInt32.zero
    )
    val tx = BaseTransaction(
      EmptyTransaction.version,
      Vector(input),
      EmptyTransaction.outputs,
      UInt32(1)
    )
    val t = BaseTxSigComponent(
      transaction = tx,
      inputIndex = TestUtil.testProgram.txSignatureComponent.inputIndex,
      output = TransactionOutput(
        CurrencyUnits.zero,
        TestUtil.testProgram.txSignatureComponent.scriptPubKey
      ),
      // standard flags include ScriptVerifyMinimalData
      flags = TestUtil.testProgram.flags
    )
    val program = PreExecutionScriptProgram(t).toExecutionInProgress
      .updateStackAndScript(stack, script)
    val newProgram = LockTimeInterpreter.opCheckLockTimeVerify(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error.isDefined must be(
      true
    )
  }

  it must "not duplicate a non-canonical zero encoding with OP_IFDUP (Bitcoin Core treats it as false)" in {
    // Finding 6 (Medium): core/src/main/scala/org/bitcoins/core/script/stack/StackInterpreter.scala:38
    // only compares the stack top against ScriptNumber.zero (the empty vector); Core's CastToBool
    // treats every all-zero byte encoding (and negative zero) as false.
    // Correct behavior: "00" is false, so OP_IFDUP leaves the stack unchanged.
    val stack = List(ScriptConstant("00"))
    val script = List(OP_IFDUP)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(
        stack,
        script
      )
    val newProgram = StackInterpreter.opIfDup(program)
    newProgram.stack must be(stack)
  }

  it must "move the nth stack element to the top with OP_ROLL even when duplicate values are on the stack" in {
    // Finding 7 (Low): core/src/main/scala/org/bitcoins/core/script/stack/StackInterpreter.scala:170-173
    // removes the rolled element with stack.tail.diff(List(newStackTop)), which removes the FIRST
    // equal element instead of the element at depth n.
    // Correct behavior (Core): the element at depth n is moved to the top.
    val a = ScriptConstant("aa")
    val b = ScriptConstant("bb")
    val stack = List(ScriptNumber(2), a, b, a)
    val script = List(OP_ROLL)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(
        stack,
        script
      )
    val newProgram = StackInterpreter.opRoll(program)
    // the second 'a' (depth 2) moves to the top; the first 'a' stays at depth 1
    newProgram.stack must be(List(a, a, b))
  }

  it must "count a scriptSig ending in OP_16 as push-only" in {
    // Finding 8 (Low): core/src/main/scala/org/bitcoins/core/util/BitcoinScriptUtil.scala:202-220
    // isPushOnly uses scriptOp.opCode < OP_16.opCode, rejecting OP_16 itself; Core's IsPushOnly
    // (script.cpp) allows all opcodes up to and including OP_16.
    // Correct behavior: OP_16 counts as a push.
    val asm = Seq(BytesToPushOntoStack(1), ScriptConstant("ff"), OP_16)
    BitcoinScriptUtil.isPushOnly(asm) must be(true)
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
