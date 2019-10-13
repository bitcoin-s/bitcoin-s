package org.bitcoins.core.script.locktime

import org.bitcoins.core.crypto.{BaseTxSigComponent, TxSigComponent}
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.{OP_0, ScriptNumber}
import org.bitcoins.core.script.result._
import org.bitcoins.core.script.{
  ExecutedScriptProgram,
  PreExecutionScriptProgram,
  ScriptProgram
}
import org.bitcoins.core.util.{ScriptProgramTestUtil, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 3/30/16.
  */
class LockTimeInterpreterTest extends FlatSpec with MustMatchers {
  val LTI = LockTimeInterpreter
  "LockTimeInterpreter" must "mark the transaction invalid if the stack is empty" in {
    val stack = Seq()
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(
      LTI.opCheckLockTimeVerify(program))
    newProgram.error must be(Some(ScriptErrorInvalidStackOperation))
  }

  it must "mark the transaction invalid if the transaction's sequence number is set to the max" in {
    val stack = Seq(OP_0)
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(
      LTI.opCheckLockTimeVerify(program))
    newProgram.error must be(Some(ScriptErrorUnsatisfiedLocktime))
  }

  it must "mark the transaction as invalid if the stack top is negative" in {
    val stack = Seq(ScriptNumber(-1))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val oldInput = TestUtil.transaction.inputs(0)
    val txInputAdjustedSequenceNumber =
      TransactionInput(oldInput.previousOutput,
                       oldInput.scriptSignature,
                       UInt32.zero)
    val emptyTx = EmptyTransaction
    val txAdjustedSequenceNumber =
      BaseTransaction(emptyTx.version,
                      Seq(txInputAdjustedSequenceNumber),
                      emptyTx.outputs,
                      emptyTx.lockTime)
    val adjustedLockTimeTx = BaseTransaction(txAdjustedSequenceNumber.version,
                                             txAdjustedSequenceNumber.inputs,
                                             txAdjustedSequenceNumber.outputs,
                                             UInt32.zero)
    val t = BaseTxSigComponent(
      transaction = adjustedLockTimeTx,
      inputIndex = TestUtil.testProgram.txSignatureComponent.inputIndex,
      output = TransactionOutput(
        CurrencyUnits.zero,
        TestUtil.testProgram.txSignatureComponent.scriptPubKey),
      flags = TestUtil.testProgram.flags
    )
    val baseProgram = PreExecutionScriptProgram(t)
    val program = ScriptProgramTestUtil.toPreExecutionScriptProgram(
      baseProgram.updateStackAndScript(stack, script))
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(
      LTI.opCheckLockTimeVerify(ScriptProgram.toExecutionInProgress(program)))
    newProgram.error must be(Some(ScriptErrorNegativeLockTime))
  }

  it must "mark the transaction as invalid if the locktime on the tx is < 500000000 && stack top is >= 500000000" in {
    val stack = Seq(ScriptNumber(500000000))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val oldInput = TestUtil.transaction.inputs(0)
    val txInputAdjustedSequenceNumber =
      TransactionInput(oldInput.previousOutput,
                       oldInput.scriptSignature,
                       UInt32.zero)
    val emptyTx = EmptyTransaction
    val txAdjustedSequenceNumber =
      BaseTransaction(emptyTx.version,
                      Seq(txInputAdjustedSequenceNumber),
                      emptyTx.outputs,
                      emptyTx.lockTime)
    val adjustedLockTimeTx = BaseTransaction(txAdjustedSequenceNumber.version,
                                             txAdjustedSequenceNumber.inputs,
                                             txAdjustedSequenceNumber.outputs,
                                             UInt32.zero)
    val t = buildTxSigComponent(adjustedLockTimeTx)
    val baseProgram = PreExecutionScriptProgram(t)
    val program = ScriptProgramTestUtil.toPreExecutionScriptProgram(
      baseProgram.updateStackAndScript(stack, script))
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(
      LTI.opCheckLockTimeVerify(ScriptProgram.toExecutionInProgress(program)))
    newProgram.error must be(Some(ScriptErrorUnsatisfiedLocktime))
  }

  it must "mark the transaction as invalid if the locktime on the tx is >= 500000000 && stack top is < 500000000" in {
    val stack = Seq(ScriptNumber(499999999))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val oldInput = TestUtil.transaction.inputs(0)
    val txInputAdjustedSequenceNumber =
      TransactionInput(oldInput.previousOutput,
                       oldInput.scriptSignature,
                       UInt32.zero)
    val emptyTx = EmptyTransaction
    val txAdjustedSequenceNumber =
      BaseTransaction(emptyTx.version,
                      Seq(txInputAdjustedSequenceNumber),
                      emptyTx.outputs,
                      emptyTx.lockTime)
    val adjustedLockTimeTx = BaseTransaction(txAdjustedSequenceNumber.version,
                                             txAdjustedSequenceNumber.inputs,
                                             txAdjustedSequenceNumber.outputs,
                                             UInt32.zero)
    val t = buildTxSigComponent(adjustedLockTimeTx)
    val baseProgram = PreExecutionScriptProgram(t)
    val program = ScriptProgramTestUtil.toPreExecutionScriptProgram(
      baseProgram.updateStackAndScript(stack, script))
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(
      LTI.opCheckLockTimeVerify(ScriptProgram.toExecutionInProgress(program)))
    newProgram.error must be(Some(ScriptErrorUnsatisfiedLocktime))
  }

  it must "mark the transaction as invalid if the stack top item is greater than the tx locktime" in {
    val stack = Seq(ScriptNumber(499999999))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val oldInput = TestUtil.transaction.inputs(0)
    val txInputAdjustedSequenceNumber =
      TransactionInput(oldInput.previousOutput,
                       oldInput.scriptSignature,
                       UInt32.zero)
    val emptyTx = EmptyTransaction
    val txAdjustedSequenceNumber =
      BaseTransaction(emptyTx.version,
                      Seq(txInputAdjustedSequenceNumber),
                      emptyTx.outputs,
                      emptyTx.lockTime)
    val adjustedLockTimeTx = BaseTransaction(txAdjustedSequenceNumber.version,
                                             txAdjustedSequenceNumber.inputs,
                                             txAdjustedSequenceNumber.outputs,
                                             UInt32.zero)
    val t = buildTxSigComponent(adjustedLockTimeTx)
    val basePreProgram = PreExecutionScriptProgram(t)
    val baseProgram = ScriptProgram.toExecutionInProgress(basePreProgram)
    val program = ScriptProgram(baseProgram, stack, script)
    val newProgram = LTI.opCheckLockTimeVerify(program)
    //if an error is hit, the newProgram will be an instance of ExecutedScriptProgram
    //if an error is not hit it will still be a ExecutionInProgressScriptProgram
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorUnsatisfiedLocktime))
  }

  it must "mark the transaction as valid if the locktime on the tx is < 500000000 && stack top is < 500000000" in {
    val stack = Seq(ScriptNumber(0))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val oldInput = TestUtil.transaction.inputs(0)
    val txInputAdjustedSequenceNumber =
      TransactionInput(oldInput.previousOutput,
                       oldInput.scriptSignature,
                       UInt32.zero)
    val emptyTx = EmptyTransaction
    val txAdjustedSequenceNumber =
      BaseTransaction(emptyTx.version,
                      Seq(txInputAdjustedSequenceNumber),
                      emptyTx.outputs,
                      emptyTx.lockTime)
    val adjustedLockTimeTx = BaseTransaction(txAdjustedSequenceNumber.version,
                                             txAdjustedSequenceNumber.inputs,
                                             txAdjustedSequenceNumber.outputs,
                                             UInt32.zero)
    val t = buildTxSigComponent(adjustedLockTimeTx)
    val basePreProgram = PreExecutionScriptProgram(t)
    val baseProgram = ScriptProgram.toExecutionInProgress(basePreProgram)
    val program = ScriptProgram(baseProgram, stack, script)
    val newProgram = LTI.opCheckLockTimeVerify(program)
    //if an error is hit, the newProgram will be an instance of ExecutedScriptProgram
    //if an error is not hit it will still be a ExecutionInProgressScriptProgram
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(false)
  }

  it must "mark the transaction as valid if the locktime on the tx is >= 500000000 && stack top is >= 500000000" in {
    val stack = Seq(ScriptNumber(500000000))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val oldInput = TestUtil.transaction.inputs(0)
    val txInputAdjustedSequenceNumber =
      TransactionInput(oldInput.previousOutput,
                       oldInput.scriptSignature,
                       UInt32.zero)
    val emptyTx = EmptyTransaction
    val txAdjustedSequenceNumber =
      BaseTransaction(emptyTx.version,
                      Seq(txInputAdjustedSequenceNumber),
                      emptyTx.outputs,
                      emptyTx.lockTime)
    val adjustedLockTimeTx = BaseTransaction(txAdjustedSequenceNumber.version,
                                             txAdjustedSequenceNumber.inputs,
                                             txAdjustedSequenceNumber.outputs,
                                             UInt32(500000000))
    val t = buildTxSigComponent(adjustedLockTimeTx)
    val basePreProgram = PreExecutionScriptProgram(t)
    val baseProgram = ScriptProgram.toExecutionInProgress(basePreProgram)
    val program = ScriptProgram(baseProgram, stack, script)
    val newProgram = LTI.opCheckLockTimeVerify(program)
    //if an error is hit, the newProgram will be an instance of ExecutedScriptProgram
    //if an error is not hit it will still be a ExecutionInProgressScriptProgram
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(false)
  }

  it must "mark the script as invalid for OP_CHECKSEQUENCEVERIFY if there are no tokens on the stack" in {
    val stack = List()
    val script = List(OP_CHECKSEQUENCEVERIFY)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = LTI.opCheckSequenceVerify(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorInvalidStackOperation))
  }

  it must "mark the script as invalid for OP_CHECKSEQUENCEVERIFY if the stack top is negative" in {
    val stack = List(ScriptNumber.negativeOne)
    val script = List(OP_CHECKSEQUENCEVERIFY)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = LTI.opCheckSequenceVerify(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorNegativeLockTime))
  }

  it must "mark the script as invalid for OP_CHECKSEQUENCEVERIFY if we are requiring minimal encoding of numbers and the stack top is not minimal" in {
    val stack = List(ScriptNumber("0100"))
    val script = List(OP_CHECKSEQUENCEVERIFY)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = LTI.opCheckSequenceVerify(program)
    newProgram.isInstanceOf[ExecutedScriptProgram] must be(true)
    newProgram.asInstanceOf[ExecutedScriptProgram].error must be(
      Some(ScriptErrorUnknownError))
  }

  it must "treat OP_CHECKSEQUENCEVERIFY as a NOP if the locktime disabled flag is set in the sequence number" in {
    val stack =
      List(ScriptNumber(TransactionConstants.locktimeDisabledFlag.toLong))
    val script = List(OP_CHECKSEQUENCEVERIFY)
    val program =
      ScriptProgram(TestUtil.testProgramExecutionInProgress, stack, script)
    val newProgram = LTI.opCheckSequenceVerify(program)
    newProgram.stack must be(stack)
    newProgram.script.isEmpty must be(true)
  }

  private def buildTxSigComponent(
      adjustedLockTimeTx: BaseTransaction): TxSigComponent = {
    val t = BaseTxSigComponent(
      transaction = adjustedLockTimeTx,
      inputIndex = TestUtil.testProgram.txSignatureComponent.inputIndex,
      output = TransactionOutput(
        CurrencyUnits.zero,
        TestUtil.testProgram.txSignatureComponent.scriptPubKey),
      flags = TestUtil.testProgram.flags
    )
    t
  }
}
