package org.bitcoins.crypto

class SignWithEntropyTest extends BitcoinSCryptoAsyncTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  //ECPrivateKey implements the sign interface
  //so just use it for testing purposes
  val privKey: Sign = ECPrivateKey.freshPrivateKey
  val pubKey: ECPublicKey = privKey.publicKey

  behavior of "SignWithEntropy"

  it must "sign arbitrary data correctly with low R values" in {
    forAllAsync(CryptoGenerators.sha256Digest) { hash =>
      val bytes = hash.bytes

      for {
        sig1 <- privKey.signLowRFuture(bytes)
        sig2 <- privKey.signLowRFuture(bytes) // Check for determinism
      } yield {
        assert(pubKey.verify(bytes, sig1))
        assert(
          sig1.bytes.length <= 70
        ) // This assertion fails if Low R is not used
        assert(sig1.bytes == sig2.bytes)
        assert(sig1 == sig2)
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
}
