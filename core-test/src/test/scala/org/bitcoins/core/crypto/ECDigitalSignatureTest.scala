package org.bitcoins.core.crypto

import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.ByteVector

/**
  * Created by chris on 3/22/16.
  */
class ECDigitalSignatureTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig = generatorDrivenConfigNewCode

  "ECDigitalSignature" must "say that empty signature is a valid DER encoded signature" in {
    val emptySiganture = ECDigitalSignature(ByteVector.empty)
    emptySiganture.isDEREncoded must be(true)

  }

  it must "say that a signature taken from a p2sh transaction is a valid DER encoded signature" in {
    val signature = ECDigitalSignature(
      "304402205b7d2c2f177ae76cfbbf14d589c113b0b35db753d305d5562dd0b61cbf366cfb02202e56f93c4f08a27f986cd424ffc48a462c3202c4902104d4d0ff98ed28f4bf80")
    signature.isDEREncoded must be(true)
  }

  it must "say that signature taken from a p2pkh transaction is a valid DER encoded signature" in {
    val signature = ECDigitalSignature(
      "3044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac")
    signature.isDEREncoded must be(true)
  }

  it must "say that a signature taken from a p2pk transaction is a valid DER encoded signature" in {
    val signature = ECDigitalSignature(
      "304402200a5c6163f07b8d3b013c4d1d6dba25e780b39658d79ba37af7057a3b7f15ffa102201fd9b4eaa9943f734928b99a83592c2e7bf342ea2680f6a2bb705167966b7420")
    signature.isDEREncoded must be(true)
  }

  it must "say that the empty digital signatures r,s values are both 0" in {
    EmptyDigitalSignature.r must be(0)
    EmptyDigitalSignature.s must be(0)
  }

  it must "create an empty digital signature when given 0 in hex or byte format" in {
    val hex = ECDigitalSignature("00")
    val byte = ECDigitalSignature(ByteVector.low(1))
    val emptySignature = ECDigitalSignature("")
    byte must be(emptySignature)
    hex must be(emptySignature)
  }

  it must "must be der encoded" in {
    forAll(CryptoGenerators.digitalSignature) { signature =>
      assert(signature.isDEREncoded)
    }
  }

  it must "must have a low s" in {
    forAll(CryptoGenerators.digitalSignature) { signature =>
      assert(DERSignatureUtil.isLowS(signature))
    }
  }

  it must "must create and verify a digital signature" in {
    forAll(CryptoGenerators.doubleSha256Digest, CryptoGenerators.privateKey) {
      case (hash, key) =>
        val sig = key.sign(hash)
        assert(key.publicKey.verify(hash, sig))
    }
  }

  it must "must not reuse r values" in {
    forAll(CryptoGenerators.privateKey,
           CryptoGenerators.doubleSha256Digest,
           CryptoGenerators.doubleSha256Digest) {
      case (key, hash1, hash2) =>
        val sig1 = key.sign(hash1)
        val sig2 = key.sign(hash2)
        assert(sig1.r != sig2.r)
    }
  }

  it must "must have serialization symmetry with r,s" in {
    forAll(CryptoGenerators.digitalSignature) {
      case sig: ECDigitalSignature =>
        val sig2 = ECDigitalSignature.fromRS(sig.r, sig.s)

        assert(sig2 == sig)
        assert(sig2.r == sig.r)
        assert(sig2.s == sig.s)
    }
  }

  it must "must have serialization symmetry toRawRS & fromRS" in {
    forAll(CryptoGenerators.digitalSignature) {
      case sig: ECDigitalSignature =>
        val raw = sig.toRawRS
        assert(ECDigitalSignature.fromRS(raw) == sig)
    }
  }

  it must "be able to generate valid signatures with bouncy castle" in {
    forAll(CryptoGenerators.privateKey, CryptoGenerators.sha256Digest) {
      case (privKey: ECPrivateKey, hash: Sha256Digest) =>
        val sig = privKey.signWithBouncyCastle(hash.bytes)
        val pubKey = privKey.publicKey

        assert(pubKey.verify(hash, sig))
    }
  }

}
