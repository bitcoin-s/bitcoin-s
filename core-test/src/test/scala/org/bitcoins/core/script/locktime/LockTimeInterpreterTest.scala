package org.bitcoins.core.script.locktime

import org.bitcoins.core.crypto.{BaseTxSigComponent, TxSigComponent}
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.{OP_0, ScriptNumber}
import org.bitcoins.core.script.result._
import org.bitcoins.core.script.{
  ExecutedScriptProgram,
  PreExecutionScriptProgram
}
import org.bitcoins.core.util.ScriptProgramTestUtil
import org.bitcoins.testkitcore.util.TestUtil
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 3/30/16.
  */
class LockTimeInterpreterTest extends BitcoinSUnitTest {
  val LTI = LockTimeInterpreter
  "LockTimeInterpreter" must "mark the transaction invalid if the stack is empty" in {
    val stack = Seq()
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(
        stack,
        script
      )
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(
      LTI.opCheckLockTimeVerify(program)
    )
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))
  }

  it must "mark the transaction invalid if the transaction's sequence number is set to the max" in {
    val stack = Seq(OP_0)
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(
        stack,
        script
      )
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(
      LTI.opCheckLockTimeVerify(program)
    )
    newProgram.error must be(Some(ScriptErrorUnsatisfiedLocktime))
  }

  it must "mark the transaction as invalid if the stack top is negative" in {
    val stack = Seq(ScriptNumber(-1))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val oldInput = TestUtil.transaction.inputs.head
    val txInputAdjustedSequenceNumber =
      TransactionInput(
        oldInput.previousOutput,
        oldInput.scriptSignature,
        UInt32.zero
      )
    val emptyTx = EmptyTransaction
    val txAdjustedSequenceNumber =
      BaseTransaction(
        emptyTx.version,
        Vector(txInputAdjustedSequenceNumber),
        emptyTx.outputs,
        emptyTx.lockTime
      )
    val adjustedLockTimeTx = BaseTransaction(
      txAdjustedSequenceNumber.version,
      txAdjustedSequenceNumber.inputs,
      txAdjustedSequenceNumber.outputs,
      UInt32.zero
    )
    val t = BaseTxSigComponent(
      transaction = adjustedLockTimeTx,
      inputIndex = TestUtil.testProgram.txSignatureComponent.inputIndex,
      output = TransactionOutput(
        CurrencyUnits.zero,
        TestUtil.testProgram.txSignatureComponent.scriptPubKey
      ),
      flags = TestUtil.testProgram.flags
    )
    val baseProgram = PreExecutionScriptProgram(t)
    val program = baseProgram.updateStackAndScript(stack, script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(
      LTI.opCheckLockTimeVerify(program.toExecutionInProgress)
    )
    newProgram.error must be(Some(ScriptErrorNegativeLockTime))
  }

  it must "mark the transaction as invalid if the locktime on the tx is < 500000000 && stack top is >= 500000000" in {
    val stack = Seq(ScriptNumber(500000000))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val oldInput = TestUtil.transaction.inputs.head
    val txInputAdjustedSequenceNumber =
      TransactionInput(
        oldInput.previousOutput,
        oldInput.scriptSignature,
        UInt32.zero
      )
    val emptyTx = EmptyTransaction
    val txAdjustedSequenceNumber =
      BaseTransaction(
        emptyTx.version,
        Vector(txInputAdjustedSequenceNumber),
        emptyTx.outputs,
        emptyTx.lockTime
      )
    val adjustedLockTimeTx = BaseTransaction(
      txAdjustedSequenceNumber.version,
      txAdjustedSequenceNumber.inputs,
      txAdjustedSequenceNumber.outputs,
      UInt32.zero
    )
    val t = buildTxSigComponent(adjustedLockTimeTx)
    val baseProgram = PreExecutionScriptProgram(t)
    val program = baseProgram.updateStackAndScript(stack, script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(
      LTI.opCheckLockTimeVerify(program.toExecutionInProgress)
    )
    newProgram.error must be(Some(ScriptErrorUnsatisfiedLocktime))
  }

  it must "mark the transaction as invalid if the locktime on the tx is >= 500000000 && stack top is < 500000000" in {
    val stack = Seq(ScriptNumber(499999999))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val oldInput = TestUtil.transaction.inputs.head
    val txInputAdjustedSequenceNumber =
      TransactionInput(
        oldInput.previousOutput,
        oldInput.scriptSignature,
        UInt32.zero
      )
    val emptyTx = EmptyTransaction
    val txAdjustedSequenceNumber =
      BaseTransaction(
        emptyTx.version,
        Vector(txInputAdjustedSequenceNumber),
        emptyTx.outputs,
        emptyTx.lockTime
      )
    val adjustedLockTimeTx = BaseTransaction(
      txAdjustedSequenceNumber.version,
      txAdjustedSequenceNumber.inputs,
      txAdjustedSequenceNumber.outputs,
      UInt32.zero
    )
    val t = buildTxSigComponent(adjustedLockTimeTx)
    val baseProgram = PreExecutionScriptProgram(t)
    val program = baseProgram.updateStackAndScript(stack, script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(
      LTI.opCheckLockTimeVerify(program.toExecutionInProgress)
    )
    newProgram.error must be(Some(ScriptErrorUnsatisfiedLocktime))
  }

  it must "mark the transaction as invalid if the stack top item is greater than the tx locktime" in {
    val stack = Seq(ScriptNumber(499999999))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val oldInput = TestUtil.transaction.inputs.head
    val txInputAdjustedSequenceNumber =
      TransactionInput(
        oldInput.previousOutput,
        oldInput.scriptSignature,
        UInt32.zero
      )
    val emptyTx = EmptyTransaction
    val txAdjustedSequenceNumber =
      BaseTransaction(
        emptyTx.version,
        Vector(txInputAdjustedSequenceNumber),
        emptyTx.outputs,
        emptyTx.lockTime
      )
    val adjustedLockTimeTx = BaseTransaction(
      txAdjustedSequenceNumber.version,
      txAdjustedSequenceNumber.inputs,
      txAdjustedSequenceNumber.outputs,
      UInt32.zero
    )
    val t = buildTxSigComponent(adjustedLockTimeTx)
    val basePreProgram = PreExecutionScriptProgram(t)
    val baseProgram = basePreProgram.toExecutionInProgress
    val program = baseProgram.updateStackAndScript(stack, script)
    val newProgram = LTI.opCheckLockTimeVerify(program)
    // if an error is hit, the newProgram will be an instance of ExecutedScriptProgram
    // if an error is not hit it will still be a ExecutionInProgressScriptProgram
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorUnsatisfiedLocktime)
    )
  }

  it must "mark the transaction as invalid if the stack top is a non-minimally encoded locktime and the minimal data flag is set" in {
    // opCheckLockTimeVerify never checked ScriptFlagUtil.requireMinimalData / isShortestEncoding on
    // its operand, unlike opCheckSequenceVerify which already enforces this for CSV. Bitcoin Core's
    // CScriptNum constructor enforces fRequireMinimal for both CLTV and CSV operands identically.
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
    val newProgram = LTI.opCheckLockTimeVerify(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error.isDefined must be(
      true
    )
  }

  it must "mark the transaction as valid if the locktime on the tx is < 500000000 && stack top is < 500000000" in {
    val stack = Seq(ScriptNumber(0))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val oldInput = TestUtil.transaction.inputs.head
    val txInputAdjustedSequenceNumber =
      TransactionInput(
        oldInput.previousOutput,
        oldInput.scriptSignature,
        UInt32.zero
      )
    val emptyTx = EmptyTransaction
    val txAdjustedSequenceNumber =
      BaseTransaction(
        emptyTx.version,
        Vector(txInputAdjustedSequenceNumber),
        emptyTx.outputs,
        emptyTx.lockTime
      )
    val adjustedLockTimeTx = BaseTransaction(
      txAdjustedSequenceNumber.version,
      txAdjustedSequenceNumber.inputs,
      txAdjustedSequenceNumber.outputs,
      UInt32.zero
    )
    val t = buildTxSigComponent(adjustedLockTimeTx)
    val basePreProgram = PreExecutionScriptProgram(t)
    val baseProgram = basePreProgram.toExecutionInProgress
    val program = baseProgram.updateStackAndScript(stack, script)
    val newProgram = LTI.opCheckLockTimeVerify(program)
    // if an error is hit, the newProgram will be an instance of ExecutedScriptProgram
    // if an error is not hit it will still be a ExecutionInProgressScriptProgram
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(false)
  }

  it must "mark the transaction as valid if the locktime on the tx is >= 500000000 && stack top is >= 500000000" in {
    val stack = Seq(ScriptNumber(500000000))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val oldInput = TestUtil.transaction.inputs.head
    val txInputAdjustedSequenceNumber =
      TransactionInput(
        oldInput.previousOutput,
        oldInput.scriptSignature,
        UInt32.zero
      )
    val emptyTx = EmptyTransaction
    val txAdjustedSequenceNumber =
      BaseTransaction(
        emptyTx.version,
        Vector(txInputAdjustedSequenceNumber),
        emptyTx.outputs,
        emptyTx.lockTime
      )
    val adjustedLockTimeTx = BaseTransaction(
      txAdjustedSequenceNumber.version,
      txAdjustedSequenceNumber.inputs,
      txAdjustedSequenceNumber.outputs,
      UInt32(500000000)
    )
    val t = buildTxSigComponent(adjustedLockTimeTx)
    val basePreProgram = PreExecutionScriptProgram(t)
    val baseProgram = basePreProgram.toExecutionInProgress
    val program = baseProgram.updateStackAndScript(stack, script)
    val newProgram = LTI.opCheckLockTimeVerify(program)
    // if an error is hit, the newProgram will be an instance of ExecutedScriptProgram
    // if an error is not hit it will still be a ExecutionInProgressScriptProgram
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(false)
  }

  it must "mark the script as invalid for OP_CHECKSEQUENCEVERIFY if there are no tokens on the stack" in {
    val stack = List()
    val script = List(OP_CHECKSEQUENCEVERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(
        stack,
        script
      )
    val newProgram = LTI.opCheckSequenceVerify(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorInvalidStackOperation)
    )
  }

  it must "mark the script as invalid for OP_CHECKSEQUENCEVERIFY if the stack top is negative" in {
    val stack = List(ScriptNumber.negativeOne)
    val script = List(OP_CHECKSEQUENCEVERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(
        stack,
        script
      )
    val newProgram = LTI.opCheckSequenceVerify(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorNegativeLockTime)
    )
  }

  it must "mark the script as invalid for OP_CHECKSEQUENCEVERIFY if the stack top is any negative value, not just negativeOne" in {
    // negatives other than -1 (e.g. -2) have the BIP68 disable flag (1 << 31) set in their
    // two's-complement bit pattern, so they must still fail with ScriptErrorNegativeLockTime
    // rather than falling through to the "disable flag set, treat as NOP" branch.
    val stack = List(ScriptNumber(-2))
    val script = List(OP_CHECKSEQUENCEVERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(
        stack,
        script
      )
    val newProgram = LTI.opCheckSequenceVerify(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorNegativeLockTime)
    )
  }

  it must "mark the script as invalid for OP_CHECKSEQUENCEVERIFY if we are requiring minimal encoding of numbers and the stack top is not minimal" in {
    val stack = List(ScriptNumber("0100"))
    val script = List(OP_CHECKSEQUENCEVERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(
        stack,
        script
      )
    val newProgram = LTI.opCheckSequenceVerify(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorUnknownError)
    )
  }

  it must "treat OP_CHECKSEQUENCEVERIFY as a NOP if the locktime disabled flag is set in the sequence number" in {
    val stack =
      List(ScriptNumber(TransactionConstants.locktimeDisabledFlag.toLong))
    val script = List(OP_CHECKSEQUENCEVERIFY)
    val program =
      TestUtil.testProgramExecutionInProgress.updateStackAndScript(
        stack,
        script
      )
    val newProgram = LTI.opCheckSequenceVerify(program)
    newProgram.stack must be(stack)
    newProgram.script.isEmpty must be(true)
  }

  private def buildTxSigComponent(
      adjustedLockTimeTx: BaseTransaction
  ): TxSigComponent = {
    val t = BaseTxSigComponent(
      transaction = adjustedLockTimeTx,
      inputIndex = TestUtil.testProgram.txSignatureComponent.inputIndex,
      output = TransactionOutput(
        CurrencyUnits.zero,
        TestUtil.testProgram.txSignatureComponent.scriptPubKey
      ),
      flags = TestUtil.testProgram.flags
    )
    t
  }
}
