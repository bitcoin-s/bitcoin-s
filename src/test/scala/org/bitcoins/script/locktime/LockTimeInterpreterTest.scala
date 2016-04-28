package org.bitcoins.script.locktime

import org.bitcoins.protocol.transaction.{TransactionInput, Transaction, UpdateTransactionInputs}
import org.bitcoins.script.error.{ScriptErrorNegativeLockTime, ScriptErrorUnsatisfiedLocktime, ScriptErrorInvalidStackOperation}
import org.bitcoins.script.{ExecutionInProgressScriptProgram, ExecutedScriptProgram, PreExecutionScriptProgram, ScriptProgram}
import org.bitcoins.script.constant.{ScriptNumber, ScriptNumberImpl, OP_0}
import org.bitcoins.util.{ScriptProgramTestUtil, TestUtil}
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 3/30/16.
 */
class LockTimeInterpreterTest extends FlatSpec with MustMatchers with LockTimeInterpreter {

  "LockTimeInterpreter" must "mark the transaction invalid if the stack is empty" in {
    val stack = Seq()
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress,stack,script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(opCheckLockTimeVerify(program))
    newProgram.error must be (Some(ScriptErrorInvalidStackOperation))
  }

  it must "mark the transaction invalid if the transaction's sequence number is set to the max" in {
    val stack = Seq(OP_0)
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val program = ScriptProgram(TestUtil.testProgramExecutionInProgress,stack,script)
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(opCheckLockTimeVerify(program))
    newProgram.error must be (Some(ScriptErrorUnsatisfiedLocktime))
  }

  it must "mark the transaction as invalid if the stack top is negative" in {
    val stack = Seq(ScriptNumber(-1))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val txInputAdjustedSequenceNumber = TransactionInput(TestUtil.transaction.inputs(0),0)
    val txAdjustedSequenceNumber = Transaction(TestUtil.transaction,UpdateTransactionInputs(Seq(txInputAdjustedSequenceNumber)))
    val adjustedLockTimeTx = Transaction(txAdjustedSequenceNumber,0)
    val baseProgram = ScriptProgram(adjustedLockTimeTx,TestUtil.testProgram.txSignatureComponent.scriptPubKey,
      TestUtil.testProgram.txSignatureComponent.inputIndex,TestUtil.testProgram.flags)
    val program = ScriptProgramTestUtil.toPreExecutionScriptProgram(ScriptProgram(baseProgram,stack,script))
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(opCheckLockTimeVerify(ScriptProgram.toExecutionInProgress(program)))
    newProgram.error must be (Some(ScriptErrorNegativeLockTime))
  }

  it must "mark the transaction as invalid if the locktime on the tx is < 500000000 && stack top is >= 500000000" in {
    val stack = Seq(ScriptNumber(500000000))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val txInputAdjustedSequenceNumber = TransactionInput(TestUtil.transaction.inputs(0),0)
    val txAdjustedSequenceNumber = Transaction(TestUtil.transaction,UpdateTransactionInputs(Seq(txInputAdjustedSequenceNumber)))
    val adjustedLockTimeTx = Transaction(txAdjustedSequenceNumber,0)
    val baseProgram = ScriptProgram(adjustedLockTimeTx,TestUtil.testProgram.txSignatureComponent.scriptPubKey,
      TestUtil.testProgram.txSignatureComponent.inputIndex,TestUtil.testProgram.flags)
    val program = ScriptProgramTestUtil.toPreExecutionScriptProgram(ScriptProgram(baseProgram,stack,script))
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(opCheckLockTimeVerify(ScriptProgram.toExecutionInProgress(program)))
    newProgram.error must be (Some(ScriptErrorUnsatisfiedLocktime))
  }

  it must "mark the transaction as invalid if the locktime on the tx is >= 500000000 && stack top is < 500000000" in {
    val stack = Seq(ScriptNumber(499999999))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val txInputAdjustedSequenceNumber = TransactionInput(TestUtil.transaction.inputs(0),0)
    val txAdjustedSequenceNumber = Transaction(TestUtil.transaction,UpdateTransactionInputs(Seq(txInputAdjustedSequenceNumber)))
    val adjustedLockTimeTx = Transaction(txAdjustedSequenceNumber,500000000)
    val baseProgram = ScriptProgram(adjustedLockTimeTx,TestUtil.testProgram.txSignatureComponent.scriptPubKey,
      TestUtil.testProgram.txSignatureComponent.inputIndex,TestUtil.testProgram.flags)
    val program = ScriptProgramTestUtil.toPreExecutionScriptProgram(ScriptProgram(baseProgram,stack,script))
    val newProgram = ScriptProgramTestUtil.toExecutedScriptProgram(opCheckLockTimeVerify(ScriptProgram.toExecutionInProgress(program)))
    newProgram.error must be (Some(ScriptErrorUnsatisfiedLocktime))
  }

  it must "mark the transaction as valid if the locktime on the tx is < 500000000 && stack top is < 500000000" in {
    val stack = Seq(ScriptNumber(499999999))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val txInputAdjustedSequenceNumber = TransactionInput(TestUtil.transaction.inputs(0),0)
    val txAdjustedSequenceNumber = Transaction(TestUtil.transaction,UpdateTransactionInputs(Seq(txInputAdjustedSequenceNumber)))
    val adjustedLockTimeTx = Transaction(txAdjustedSequenceNumber,0)
    val baseProgram = ScriptProgram(adjustedLockTimeTx,TestUtil.testProgram.txSignatureComponent.scriptPubKey,
      TestUtil.testProgram.txSignatureComponent.inputIndex,TestUtil.testProgram.flags)
    val program = ScriptProgram(baseProgram,stack,script)
    val newProgram = opCheckLockTimeVerify(program)
    //if an error is hit, the newProgram will be an instance of ExecutedScriptProgram
    //if an error is not hit it will still be a ExecutionInProgressScriptProgram
    newProgram.isInstanceOf[ExecutedScriptProgram] must be (false)
  }

  it must "mark the transaction as valid if the locktime on the tx is >= 500000000 && stack top is >= 500000000" in {
    val stack = Seq(ScriptNumber(500000000))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val txInputAdjustedSequenceNumber = TransactionInput(TestUtil.transaction.inputs(0),0)
    val txAdjustedSequenceNumber = Transaction(TestUtil.transaction,UpdateTransactionInputs(Seq(txInputAdjustedSequenceNumber)))
    val adjustedLockTimeTx = Transaction(txAdjustedSequenceNumber,500000000)
    val baseProgram : PreExecutionScriptProgram = ScriptProgram(adjustedLockTimeTx,TestUtil.testProgram.txSignatureComponent.scriptPubKey,
      TestUtil.testProgram.txSignatureComponent.inputIndex,TestUtil.testProgram.flags)
    val program = ScriptProgram(baseProgram,stack,script)
    val newProgram = opCheckLockTimeVerify(program)
    //if an error is hit, the newProgram will be an instance of ExecutedScriptProgram
    //if an error is not hit it will still be a ExecutionInProgressScriptProgram
    newProgram.isInstanceOf[ExecutedScriptProgram] must be (false)
  }
}

