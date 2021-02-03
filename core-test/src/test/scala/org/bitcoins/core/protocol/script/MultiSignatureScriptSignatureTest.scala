package org.bitcoins.core.protocol.script

import org.bitcoins.testkit.util.{BitcoinSUnitTest, TransactionTestUtil}

/** Created by chris on 3/8/16.
  */
class MultiSignatureScriptSignatureTest extends BitcoinSUnitTest {

  "MultiSignatureScriptSignature" must "find all of the digital signatures for a multisignature scriptSig" in {
    val (spendingTx, inputIndex, _, _) =
      TransactionTestUtil.signedMultiSignatureTransaction
    val scriptSig = spendingTx.inputs(inputIndex).scriptSignature
    scriptSig.signatures.size must be(2)
  }

  it must "Fail validation if asm is empty" in {
    assert(
      !MultiSignatureScriptSignature.isMultiSignatureScriptSignature(
        Vector.empty))
  }

}
