package org.bitcoins.protocol.script

import org.bitcoins.crypto.ECFactory
import org.bitcoins.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 4/1/16.
 */
class P2PKScriptPubKeyTest extends FlatSpec with MustMatchers {

  "P2PKScriptPubKeyTest" must "find the public key in a p2pk scriptPubKey" in {
    val p2pkScriptPubKey = TestUtil.p2pkScriptPubKey match {
      case s : P2PKScriptPubKey => s
      case _ => throw new RuntimeException("should have been p2pk script pub key")
    }

    p2pkScriptPubKey.publicKey must be (ECFactory.publicKey("0479be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8"))
  }
}
