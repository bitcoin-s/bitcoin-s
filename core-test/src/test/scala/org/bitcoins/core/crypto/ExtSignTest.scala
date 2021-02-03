package org.bitcoins.core.crypto

import org.bitcoins.testkit.core.gen.{CryptoGenerators, HDGenerators}
import org.bitcoins.testkit.util.BitcoinSAsyncTest

class ExtSignTest extends BitcoinSAsyncTest {

  it must "be able to sign something that extends ExtSignKey" in {
    forAll(CryptoGenerators.extPrivateKey, CryptoGenerators.sha256Digest) {
      case (extPrivKey, hash) =>
        val sig = extPrivKey.sign(hash.bytes)
        assert(extPrivKey.publicKey.verify(hash, sig))
    }
  }

  it must "be able to sign a specific path of a ext key" in {
    forAllAsync(CryptoGenerators.extPrivateKey,
                CryptoGenerators.sha256Digest,
                HDGenerators.bip32Path) { case (extPrivKey, hash, path) =>
      val sigF = extPrivKey.deriveAndSignFuture(hash.bytes, path)
      val childPubKey = extPrivKey.deriveChildPubKey(path).get
      sigF.map { sig =>
        assert(childPubKey.key.verify(hash, sig))
      }

    }
  }
}
