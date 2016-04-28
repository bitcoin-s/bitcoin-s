package org.bitcoins.protocol.script

import org.bitcoins.crypto.ECFactory
import org.bitcoins.script.crypto.SIGHASH_ALL
import org.bitcoins.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 4/1/16.
 */
class P2PKHScriptSignatureTest extends FlatSpec with MustMatchers {

  "P2PKHScriptSignature" must "be able to identify it's own hash type" in {
    val p2pkhScriptSig = TestUtil.p2pkhScriptSig match {
      case s : P2PKHScriptSignature => s
      case _ => throw new RuntimeException("Must be p2pkh scriptSig")
    }
    p2pkhScriptSig.hashType must be (SIGHASH_ALL(1))
  }

  it must "be able to identify the signature in a p2pkh scriptSig" in {
    val p2pkhScriptSig = TestUtil.p2pkhScriptSig match {
      case s : P2PKHScriptSignature => s
      case _ => throw new RuntimeException("Must be p2pkh scriptSig")
    }
    p2pkhScriptSig.signature must be (ECFactory.digitalSignature("3044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac01"))
  }

}
