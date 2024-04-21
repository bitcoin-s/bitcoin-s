package org.bitcoins.core.protocol.script

import org.bitcoins.testkitcore.gen.ScriptGenerators
import org.bitcoins.testkitcore.util.{BitcoinSJvmTest, TransactionTestUtil}

/** Created by chris on 3/8/16.
  */
class MultiSignatureScriptSignatureTest extends BitcoinSJvmTest {

  "MultiSignatureScriptSignature" must "find all of the digital signatures for a multisignature scriptSig" in {
    val (spendingTx, inputIndex, _, _) =
      TransactionTestUtil.signedMultiSignatureTransaction
    val scriptSig = spendingTx.inputs(inputIndex).scriptSignature
    scriptSig.signatures.size must be(2)
  }

  it must "Fail validation if asm is empty" in {
    assert(!MultiSignatureScriptSignature.isValidAsm(Vector.empty))
  }

  it must "serialization symmetry" in {
    forAll(ScriptGenerators.multiSignatureScriptSignature) {
      multiSigScriptSig =>
        assert(
          MultiSignatureScriptSignature(
            multiSigScriptSig.hex
          ) == multiSigScriptSig
        )
    }
  }
}
