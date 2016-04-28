package org.bitcoins.protocol.script

import org.bitcoins.crypto.{EmptyDigitalSignature, ECFactory}
import org.bitcoins.script.constant.{ScriptConstantImpl, BytesToPushOntoStackImpl, OP_0}
import org.bitcoins.util.TransactionTestUtil
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

  it must "give us the empty signature back when it is encoded as an OP_0 (this pushes an empty signature onto the stack)" in {
    val multiSigScriptSignature = ScriptSignature.fromAsm(List(OP_0, BytesToPushOntoStackImpl(71),
      ScriptConstantImpl("30440220b119d67d389315308d1745f734a51ff3ec72e06081e84e236fdf9dc2f5d2a64802204b04e3bc38674c4422ea317231d642b56dc09d214a1ecbbf16ecca01ed996e2201"), OP_0))
   multiSigScriptSignature.signatures must be (Seq(
     ECFactory.digitalSignature("30440220b119d67d389315308d1745f734a51ff3ec72e06081e84e236fdf9dc2f5d2a64802204b04e3bc38674c4422ea317231d642b56dc09d214a1ecbbf16ecca01ed996e2201"),
     EmptyDigitalSignature))
  }
}
