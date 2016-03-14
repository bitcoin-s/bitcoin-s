package org.scalacoin.protocol.script

import org.scalacoin.util.TransactionTestUtil
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 3/8/16.
 */
class MultiSignatureScriptSignatureTest extends FlatSpec with MustMatchers {

  "MultiSignatureScriptSignature" must "find all of the digital signatures for a multisignature scriptSig" in {
    val (spendingTx,inputIndex,_,_) = TransactionTestUtil.signedMultiSignatureTransaction
    val scriptSig = spendingTx.inputs(inputIndex).scriptSignature
    scriptSig.signatures.size must be (2)
  }
}
