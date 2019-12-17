package org.bitcoins.core.crypto

import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.util.BitcoinSUnitTest

class ExtSignTest extends BitcoinSUnitTest {

  it must "be able to sign something that extends ExtSignKey" in {
    forAll(CryptoGenerators.extPrivateKey, CryptoGenerators.sha256Digest) {
      case (extPrivKey, hash) =>
        val sig = extPrivKey.sign(hash.bytes)
        assert(extPrivKey.publicKey.verify(hash,sig))
    }
  }
}
