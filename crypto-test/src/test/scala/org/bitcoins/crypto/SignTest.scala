package org.bitcoins.crypto

import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.util.BitcoinSAsyncTest

class SignTest extends BitcoinSAsyncTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  //ECPrivateKey implements the sign interface
  //so just use it for testing purposes
  val privKey: Sign = ECPrivateKey.freshPrivateKey
  val pubKey: ECPublicKey = privKey.publicKey

  behavior of "Sign"

  it must "sign arbitrary pieces of data correctly" in {
    forAllAsync(CryptoGenerators.sha256Digest) { hash =>
      val sigF = privKey.signFunction(hash.bytes)

      sigF.map { sig =>
        assert(pubKey.verify(hash.bytes, sig))
      }
    }
  }

  it must "sign arbitrary pieces of data with arbitrary entropy correctly" in {
    forAllAsync(CryptoGenerators.sha256Digest, CryptoGenerators.sha256Digest) {
      case (hash, entropy) =>
        val sigF = privKey.signWithEntropyFunction(hash.bytes, entropy.bytes)

        sigF.map { sig =>
          assert(pubKey.verify(hash.bytes, sig))
        }
    }
  }

  it must "sign arbitrary data correctly with low R values" in {
    forAllAsync(CryptoGenerators.sha256Digest) { hash =>
      val bytes = hash.bytes
      val sigF1 = privKey.signLowRFuture(bytes)
      val sigF2 = privKey.signLowRFuture(bytes) // Check for determinism

      sigF1.flatMap { sig1 =>
        assert(pubKey.verify(bytes, sig1))
        assert(
          sig1.bytes.length <= 70
        ) // This assertion fails if Low R is not used

        sigF2.map { sig2 =>
          assert(sig1.bytes == sig2.bytes)
          assert(sig1 == sig2)
        }
      }
    }
  }
}
