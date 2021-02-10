package org.bitcoins.core.crypto

import org.bitcoins.testkitcore.gen.{CryptoGenerators, HDGenerators}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class ExtSignTest extends BitcoinSUnitTest {

  it must "be able to sign something that extends ExtSignKey" in {
    forAll(CryptoGenerators.extPrivateKey, CryptoGenerators.sha256Digest) {
      case (extPrivKey, hash) =>
        val sig = extPrivKey.sign(hash.bytes)
        assert(extPrivKey.publicKey.verify(hash, sig))
    }
  }

  it must "be able to sign a specific path of a ext key" in {
    forAll(CryptoGenerators.extPrivateKey,
           CryptoGenerators.sha256Digest,
           HDGenerators.bip32Path) { case (extPrivKey, hash, path) =>
      val sig = extPrivKey.deriveAndSign(hash.bytes, path)
      val childPubKey = extPrivKey.deriveChildPubKey(path).get
      assert(childPubKey.key.verify(hash, sig))
    }
  }
}
