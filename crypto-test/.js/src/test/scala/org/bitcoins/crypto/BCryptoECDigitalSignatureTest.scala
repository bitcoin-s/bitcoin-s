package org.bitcoins.crypto

class BCryptoECDigitalSignatureTest extends BitcoinSCryptoTest {

  behavior of "BCryptoECDigitalSignatureTest"

  it must "be able to generate valid signatures with bcrypto" in {
    forAll(CryptoGenerators.privateKey, CryptoGenerators.sha256Digest) {
      case (privKey: ECPrivateKey, hash: Sha256Digest) =>
        val sig = BCryptoCryptoRuntime.sign(privKey, hash.bytes)
        val pubKey = privKey.publicKey

        assert(pubKey.verify(hash, sig))
    }
  }
}
