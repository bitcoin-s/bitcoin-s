package org.bitcoins.core.protocol.script

import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.testkit.util.BitcoinSUnitTest

class P2WPKHWitnessSPKV0Test extends BitcoinSUnitTest {

  "P2WPKHWitnessSPKV0" must "fail to be created with an uncompressed public key" in {
    val uncompressed = ECPrivateKey(false).publicKey
    intercept[IllegalArgumentException] {
      P2WPKHWitnessSPKV0(uncompressed)
    }
  }
}
