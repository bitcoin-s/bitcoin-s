package org.bitcoins.core.protocol.script

import org.bitcoins.core.util.TransactionTestUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 3/8/16.
  */
class MultiSignatureScriptSignatureTest extends BitcoinSUnitTest {

  "MultiSignatureScriptSignature" must "find all of the digital signatures for a multisignature scriptSig" in {
    val (spendingTx, inputIndex, _, _) =
      TransactionTestUtil.signedMultiSignatureTransaction
    val scriptSig = spendingTx.inputs(inputIndex).scriptSignature
    scriptSig.signatures.size must be(2)
  }

}
