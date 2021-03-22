package org.bitcoins.crypto

class ECPrivateKeyTest extends BitcoinSCryptoTest {
  it must "create a private key from its hex representation" in {
    val privateKeyHex =
      "180cb41c7c600be951b5d3d0a7334acc7506173875834f7a6c4c786a28fcbb19"
    val key: ECPrivateKey = ECPrivateKey(privateKeyHex)
    key.hex must be(privateKeyHex)
  }

  it must "create a fresh private key" in {
    ECPrivateKey() must not equal (ECPrivateKey())
  }

  it must "have serialization symmetry" in {
    forAll(CryptoGenerators.privateKey) { privKey =>
      assert(ECPrivateKey(privKey.hex) == privKey)
      assert(ECPrivateKey.fromFieldElement(privKey.fieldElement) == privKey)
    }
  }

  it must "generate unique keys" in {
    forAll(CryptoGenerators.privateKey, CryptoGenerators.privateKey) {
      (privKey1, privKey2) =>
        assert(privKey1 != privKey2)
    }
  }

  it must "not serialize a ECPrivateKey toString" in {
    ECPrivateKey().toString must be("Masked(ECPrivateKeyImpl)")
  }

  it must "successfully negate itself" in {
    forAll(CryptoGenerators.nonZeroPrivKey) { privKey =>
      val negPrivKey = privKey.negate
      val pubKey = privKey.publicKey
      val negPubKey = negPrivKey.publicKey
      assert(pubKey.bytes.tail == negPubKey.bytes.tail)
      assert(pubKey.bytes.head != negPubKey.bytes.head)
      assert(
        privKey.fieldElement.add(negPrivKey.fieldElement) == FieldElement.zero)
    }
  }

  it must "correctly execute the ecdsa single signer adaptor signature protocol" in {
    forAll(CryptoGenerators.privateKey,
           CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32)) {
      case (privKey, adaptorSecret, msg) =>
        val adaptorSig = privKey.adaptorSign(adaptorSecret.publicKey, msg)
        assert(
          privKey.publicKey
            .adaptorVerify(msg, adaptorSecret.publicKey, adaptorSig))
        val sig = adaptorSecret.completeAdaptorSignature(adaptorSig)
        val secret =
          adaptorSecret.publicKey.extractAdaptorSecret(adaptorSig, sig)
        assert(secret == adaptorSecret)
        assert(privKey.publicKey.verify(msg, sig))
    }
  }
}
