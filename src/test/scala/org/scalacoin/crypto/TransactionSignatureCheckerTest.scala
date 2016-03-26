package org.scalacoin.crypto

import org.scalacoin.protocol.script.{ScriptSignatureFactory, ScriptSignature}
import org.scalacoin.protocol.transaction.{UpdateTransactionInputs, TransactionInput, Transaction, TransactionOutput}
import org.scalacoin.util._
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/29/16.
 */
class TransactionSignatureCheckerTest extends FlatSpec with MustMatchers {

  "TransactionSignatureChecker" must "check to see if an input correctly spends a p2pkh scriptPubKey" in {
    val (spendingTx,spendingInput,inputIndex,creditingOutput) : (Transaction,TransactionInput,Int,TransactionOutput) =
      TransactionTestUtil.transactionWithSpendingInputAndCreditingOutput
    val scriptSig : ScriptSignature = spendingInput.scriptSignature
    val pubKey : ECPublicKey = ECFactory.publicKey(scriptSig.asm.last.bytes)
    TransactionSignatureChecker.checkSignature(spendingTx,inputIndex,creditingOutput.scriptPubKey,
      pubKey,true) must be (SignatureValidationSuccess)
  }


  it must "check to see if an input spends a multisignature scriptPubKey correctly" in {
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
    val newInput = spendingTx.inputs(inputIndex).factory(newScriptSigWithSignatureRemoved)
    val txNewInputs = Transaction.factory(UpdateTransactionInputs(Seq(newInput)))
    TransactionSignatureChecker.checkSignature(txNewInputs,inputIndex,creditingOutput.scriptPubKey,true) must be (SignatureValidationfailureIncorrectSignatures)
  }
}
