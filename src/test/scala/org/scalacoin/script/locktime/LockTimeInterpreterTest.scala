package org.scalacoin.script.locktime

import org.scalacoin.protocol.transaction.{UpdateTransactionInputs, TransactionInputFactory, TransactionFactory}
import org.scalacoin.script.ScriptProgramFactory
import org.scalacoin.script.constant.{ScriptNumberImpl, OP_0}
import org.scalacoin.util.TestUtil
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 3/30/16.
 */
class LockTimeInterpreterTest extends FlatSpec with MustMatchers with LockTimeInterpreter {

  "LockTimeInterpreter" must "mark the transaction invalid if the stack is empty" in {
    val stack = Seq()
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram,stack,script)
    val newProgram = opCheckLockTimeVerify(program)
    newProgram.isValid must be (false)
  }

  it must "mark the transaction invalid if the transaction's sequence number is set to the max" in {
    val stack = Seq(OP_0)
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val program = ScriptProgramFactory.factory(TestUtil.testProgram,stack,script)
    val newProgram = opCheckLockTimeVerify(program)
    newProgram.isValid must be (false)
  }

  it must "mark the transaction as invalid if the stack top is negative" in {
    val stack = Seq(ScriptNumberImpl(-1))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val txInputAdjustedSequenceNumber = TransactionInputFactory.factory(TestUtil.transaction.inputs(0),0)
    val txAdjustedSequenceNumber = TransactionFactory.factory(TestUtil.transaction,UpdateTransactionInputs(Seq(txInputAdjustedSequenceNumber)))
    val adjustedLockTimeTx = TransactionFactory.factory(txAdjustedSequenceNumber,0)
    val baseProgram = ScriptProgramFactory.factory(adjustedLockTimeTx,TestUtil.testProgram.txSignatureComponent.scriptPubKey,
      TestUtil.testProgram.txSignatureComponent.inputIndex,TestUtil.testProgram.flags)
    val program = ScriptProgramFactory.factory(baseProgram,stack,script)
    val newProgram = opCheckLockTimeVerify(program)
    newProgram.isValid must be (false)
  }

  it must "mark the transaction as invalid if the locktime on the tx is < 500000000 && stack top is >= 500000000" in {
    val stack = Seq(ScriptNumberImpl(500000000))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val txInputAdjustedSequenceNumber = TransactionInputFactory.factory(TestUtil.transaction.inputs(0),0)
    val txAdjustedSequenceNumber = TransactionFactory.factory(TestUtil.transaction,UpdateTransactionInputs(Seq(txInputAdjustedSequenceNumber)))
    val adjustedLockTimeTx = TransactionFactory.factory(txAdjustedSequenceNumber,0)
    val baseProgram = ScriptProgramFactory.factory(adjustedLockTimeTx,TestUtil.testProgram.txSignatureComponent.scriptPubKey,
      TestUtil.testProgram.txSignatureComponent.inputIndex,TestUtil.testProgram.flags)
    val program = ScriptProgramFactory.factory(baseProgram,stack,script)
    val newProgram = opCheckLockTimeVerify(program)
    newProgram.isValid must be (false)
  }

  it must "mark the transaction as invalid if the locktime on the tx is >= 500000000 && stack top is < 500000000" in {
    val stack = Seq(ScriptNumberImpl(499999999))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val txInputAdjustedSequenceNumber = TransactionInputFactory.factory(TestUtil.transaction.inputs(0),0)
    val txAdjustedSequenceNumber = TransactionFactory.factory(TestUtil.transaction,UpdateTransactionInputs(Seq(txInputAdjustedSequenceNumber)))
    val adjustedLockTimeTx = TransactionFactory.factory(txAdjustedSequenceNumber,500000000)
    val baseProgram = ScriptProgramFactory.factory(adjustedLockTimeTx,TestUtil.testProgram.txSignatureComponent.scriptPubKey,
      TestUtil.testProgram.txSignatureComponent.inputIndex,TestUtil.testProgram.flags)
    val program = ScriptProgramFactory.factory(baseProgram,stack,script)
    val newProgram = opCheckLockTimeVerify(program)
    newProgram.isValid must be (false)
  }

  it must "mark the transaction as valid if the locktime on the tx is < 500000000 && stack top is < 500000000" in {
    val stack = Seq(ScriptNumberImpl(499999999))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val txInputAdjustedSequenceNumber = TransactionInputFactory.factory(TestUtil.transaction.inputs(0),0)
    val txAdjustedSequenceNumber = TransactionFactory.factory(TestUtil.transaction,UpdateTransactionInputs(Seq(txInputAdjustedSequenceNumber)))
    val adjustedLockTimeTx = TransactionFactory.factory(txAdjustedSequenceNumber,0)
    val baseProgram = ScriptProgramFactory.factory(adjustedLockTimeTx,TestUtil.testProgram.txSignatureComponent.scriptPubKey,
      TestUtil.testProgram.txSignatureComponent.inputIndex,TestUtil.testProgram.flags)
    val program = ScriptProgramFactory.factory(baseProgram,stack,script)
    val newProgram = opCheckLockTimeVerify(program)
    newProgram.isValid must be (true)
  }

  it must "mark the transaction as valid if the locktime on the tx is >= 500000000 && stack top is >= 500000000" in {
    val stack = Seq(ScriptNumberImpl(500000000))
    val script = Seq(OP_CHECKLOCKTIMEVERIFY)
    val txInputAdjustedSequenceNumber = TransactionInputFactory.factory(TestUtil.transaction.inputs(0),0)
    val txAdjustedSequenceNumber = TransactionFactory.factory(TestUtil.transaction,UpdateTransactionInputs(Seq(txInputAdjustedSequenceNumber)))
    val adjustedLockTimeTx = TransactionFactory.factory(txAdjustedSequenceNumber,500000000)
    val baseProgram = ScriptProgramFactory.factory(adjustedLockTimeTx,TestUtil.testProgram.txSignatureComponent.scriptPubKey,
      TestUtil.testProgram.txSignatureComponent.inputIndex,TestUtil.testProgram.flags)
    val program = ScriptProgramFactory.factory(baseProgram,stack,script)
    val newProgram = opCheckLockTimeVerify(program)
    newProgram.isValid must be (true)
  }
}

