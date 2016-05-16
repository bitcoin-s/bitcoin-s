package org.bitcoins.crypto

import org.bitcoins.policy.Policy
import org.bitcoins.protocol.script.ScriptSignature
import org.bitcoins.protocol.transaction._
import org.bitcoins.script.constant.ScriptConstant
import org.bitcoins.script.flag.ScriptVerifyDerSig
import org.bitcoins.util._
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/29/16.
 */
class TransactionSignatureCheckerTest extends FlatSpec with MustMatchers {

  "TransactionSignatureChecker" must "remove the signatures from a p2sh scriptSig" in {
    val p2shScriptSig = TestUtil.p2sh2Of3ScriptSig
    val signatures = p2shScriptSig.signatures
    val asmWithoutSigs = TransactionSignatureChecker.removeSignaturesFromScript(signatures,p2shScriptSig.asm)
    val sigExists = signatures.map(sig => asmWithoutSigs.exists(_ == ScriptConstant(sig.hex)))
    sigExists.exists(_ == true) must be (false)
  }
}
