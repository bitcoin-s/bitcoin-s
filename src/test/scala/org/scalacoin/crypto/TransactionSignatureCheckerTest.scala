package org.scalacoin.crypto

import org.scalacoin.protocol.script.{ScriptSignatureFactory, ScriptSignature}
import org.scalacoin.protocol.transaction._
import org.scalacoin.script.ScriptProgramFactory
import org.scalacoin.util._
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/29/16.
 */
class TransactionSignatureCheckerTest extends FlatSpec with MustMatchers {

  "TransactionSignatureChecker" must "check to see if an input spends a multisignature scriptPubKey correctly" in {
    val (spendingTx,inputIndex,multiSigScriptPubKey,keys) = TransactionTestUtil.signedMultiSignatureTransaction
    TransactionSignatureChecker.checkSignature(spendingTx,inputIndex,multiSigScriptPubKey,true) must be (SignatureValidationSuccess)
  }

  it must "check to see if an input spends a p2sh scriptPubKey correctly" in {
    val (spendingTx,input,inputIndex,creditingOutput) = TransactionTestUtil.p2shTransactionWithSpendingInputAndCreditingOutput
    TransactionSignatureChecker.checkSignature(spendingTx,inputIndex,creditingOutput.scriptPubKey,true) must be (SignatureValidationSuccess)
  }

  it must "check a 2/3 p2sh input script correctly" in  {
    val (spendingTx,input,inputIndex,creditingOutput) =  TransactionTestUtil.p2sh2Of3TransactionWithSpendingInputAndCreditingOutput
    TransactionSignatureChecker.checkSignature(spendingTx,inputIndex,creditingOutput.scriptPubKey,true) must be (SignatureValidationSuccess)

  }

  it must "fail to validate a 2/3 p2sh input script if a digital signature is removed" in {
    val (spendingTx,input,inputIndex,creditingOutput) =  TransactionTestUtil.p2sh2Of3TransactionWithSpendingInputAndCreditingOutput
    val scriptSig : ScriptSignature = spendingTx.inputs.head.scriptSignature
    val newScriptSigAsm = Seq(scriptSig.asm.head) ++ scriptSig.asm.slice(3,scriptSig.asm.size)
    val newScriptSigWithSignatureRemoved = ScriptSignatureFactory.fromAsm(newScriptSigAsm)
    val newInput = TransactionInputFactory.factory(spendingTx.inputs(inputIndex),newScriptSigWithSignatureRemoved)
    val txNewInputs = TransactionFactory.factory(EmptyTransaction,UpdateTransactionInputs(Seq(newInput)))
    TransactionSignatureChecker.checkSignature(txNewInputs,inputIndex,creditingOutput.scriptPubKey,true) must be (SignatureValidationFailureIncorrectSignatures)
  }

  it must "fail to check a transaction when strict der encoding is required but the signature is not strict der encoded" in {
    val (tx,inputIndex) = TransactionTestUtil.transactionWithNonStrictDerSignature
    val signature = tx.inputs(inputIndex).scriptSignature.signatures.head
    val result = TransactionSignatureChecker.checkSignature(tx,inputIndex,TestUtil.scriptPubKey,
      ECFactory.publicKey(""), signature, true)
    result must be (SignatureValidationFailureNotStrictDerEncoding)
  }


  it must "check a standard p2pk transaction" in {
    val (creditingTx,outputIndex) = TransactionTestUtil.buildCreditingTransaction(TestUtil.p2pkScriptPubKey)
    val (spendingTx,inputIndex) =
      TransactionTestUtil.buildSpendingTransaction(creditingTx,TestUtil.p2pkScriptSig,outputIndex)
    val program = ScriptProgramFactory.factory(spendingTx,TestUtil.p2pkScriptPubKey,inputIndex,Seq())
    val result = TransactionSignatureChecker.checkSignature(program)
    result must be (SignatureValidationSuccess)

  }
}
