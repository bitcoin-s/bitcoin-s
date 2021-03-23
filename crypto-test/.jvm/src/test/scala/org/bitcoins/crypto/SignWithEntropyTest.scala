package org.bitcoins.crypto

class SignWithEntropyTest extends BitcoinSCryptoTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  //ECPrivateKey implements the sign interface
  //so just use it for testing purposes
  val privKey: Sign = ECPrivateKey.freshPrivateKey
  val pubKey: ECPublicKey = privKey.publicKey

  behavior of "SignWithEntropy"

  it must "sign arbitrary data correctly with low R values" in {
    forAll(CryptoGenerators.sha256Digest) { hash =>
      val bytes = hash.bytes

      val sig1 = privKey.signLowR(bytes)
      val sig2 = privKey.signLowR(bytes) // Check for determinism
      assert(pubKey.verify(bytes, sig1))
      assert(
        sig1.bytes.length <= 70
      ) // This assertion fails if Low R is not used
      assert(sig1.bytes == sig2.bytes)
      assert(sig1 == sig2)
    }
  }

  it must "sign arbitrary pieces of data with arbitrary entropy correctly" in {
    forAll(CryptoGenerators.sha256Digest, CryptoGenerators.sha256Digest) {
      case (hash, entropy) =>
        val sig = privKey.signWithEntropy(hash.bytes, entropy.bytes)

        assert(pubKey.verify(hash.bytes, sig))
    }
  }
}
