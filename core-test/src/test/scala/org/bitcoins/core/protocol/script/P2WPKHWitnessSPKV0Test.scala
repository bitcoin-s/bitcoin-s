package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.ECPrivateKey
import org.scalatest.{FlatSpec, MustMatchers}

class P2WPKHWitnessSPKV0Test extends FlatSpec with MustMatchers {

  "P2WPKHWitnessSPKV0" must "fail to be created with an uncompressed public key" in {
    val uncompressed = ECPrivateKey(false).publicKey
    intercept[IllegalArgumentException] {
      P2WPKHWitnessSPKV0(uncompressed)
    }
  }
}
