package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.script.constant.ScriptNumber
import org.scalatest.{ FlatSpec, MustMatchers }

class P2WSHWitnessSPKV0Test extends FlatSpec with MustMatchers {
  val uncompressed = ECPrivateKey(false).publicKey
  val p2pk = P2PKScriptPubKey(uncompressed)
  val multisig = MultiSignatureScriptPubKey(1, Vector(uncompressed))
  "P2WPKHWitnessSPKV0" must "fail to be created with an uncompressed public key" in {
    intercept[IllegalArgumentException] {
      P2WSHWitnessSPKV0(p2pk)
    }
  }

  it must "fail for a multisig script with an uncompressed public key" in {

    intercept[IllegalArgumentException] {
      P2WSHWitnessSPKV0(multisig)
    }
  }

  it must "fail with a locktime script" in {
    val cltv = CLTVScriptPubKey(ScriptNumber.zero, multisig)
    intercept[IllegalArgumentException] {
      P2WSHWitnessSPKV0(cltv)
    }
  }
}
