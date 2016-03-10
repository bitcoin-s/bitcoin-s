package org.scalacoin.crypto

import org.bitcoinj.core.Transaction.SigHash
import org.bitcoinj.core.{Utils, Sha256Hash, ECKey}
import org.bitcoinj.crypto.TransactionSignature
import org.bitcoinj.params.TestNet3Params
import org.bitcoinj.script.{Script, ScriptBuilder}
import org.scalacoin.protocol.script.ScriptSignature
import org.scalacoin.protocol.transaction.{TransactionInput, Transaction, TransactionOutput}
import org.scalacoin.script.crypto.SIGHASH_ALL
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
      pubKey) must be (true)
  }


  it must "check to see if an input spends a multisignature scriptPubKey correctly" in {
    val (spendingTx,inputIndex,multiSigScriptPubKey,keys) = TransactionTestUtil.signedMultiSignatureTransaction
    TransactionSignatureChecker.checkSignature(spendingTx,inputIndex,multiSigScriptPubKey) must be (true)
  }

  it must "check to see if an input spends a p2sh scriptPubKey correctly" in {
    val (spendingTx,input,inputIndex,creditingOutput) = TransactionTestUtil.p2shTransactionWithSpendingInputAndCreditingOutput
    TransactionSignatureChecker.checkSignature(spendingTx,inputIndex,creditingOutput.scriptPubKey) must be (true)
  }
}
