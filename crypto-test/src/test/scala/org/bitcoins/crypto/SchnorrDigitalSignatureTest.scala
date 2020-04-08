package org.bitcoins.crypto

import org.bitcoins.core.util.NumberUtil
import org.bitcoins.testkit.core.gen.{CryptoGenerators, NumberGenerator}
import org.bitcoins.testkit.util.BitcoinSUnitTest

class SchnorrDigitalSignatureTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "SchnorrDigitalSignature"

  it must "have serialization symmetry" in {
    forAll(CryptoGenerators.schnorrDigitalSignature) { sig =>
      assert(SchnorrDigitalSignature(sig.bytes) == sig)
    }
  }

  it must "must create and verify a digital signature" in {
    forAll(NumberGenerator.bytevector(32), CryptoGenerators.privateKey) {
      case (bytes, privKey) =>
        val sig = privKey.schnorrSign(bytes)
        assert(privKey.publicKey.schnorrVerify(bytes, sig))
    }
  }

  it must "must not reuse R values" in {
    forAll(CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32),
           NumberGenerator.bytevector(32)) {
      case (privKey, bytes1, bytes2) =>
        val sig1 = privKey.schnorrSign(bytes1)
        val sig2 = privKey.schnorrSign(bytes2)
        assert(sig1.bytes != sig2.bytes)
        assert(sig1.rx != sig2.rx)
    }
  }

  it must "generate R values correctly" in {
    forAll(CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32),
           NumberGenerator.bytevector(32)) {
      case (privKey, auxRand, bytes) =>
        val nonce = SchnorrNonce.kFromBipSchnorr(privKey, bytes, auxRand)

        val sig1 = privKey.schnorrSign(bytes, auxRand)
        val sig2 = privKey.schnorrSignWithNonce(bytes, nonce)

        assert(sig1 == sig2)
    }
  }

  it must "correctly compute signature points" in {
    forAll(CryptoGenerators.privateKey, NumberGenerator.bytevector(32)) {
      case (privKey, data) =>
        val pubKey = privKey.publicKey
        val sig = privKey.schnorrSign(data)

        val sigPoint = pubKey.schnorrComputePoint(data, sig.rx)
        assert(sigPoint == sig.sig.toPrivateKey.publicKey)
    }
  }

  it must "correctly compute signature points for sigs with fixed nonces" in {
    forAll(CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32),
           CryptoGenerators.privateKey) {
      case (privKey, data, nonce) =>
        val pubKey = privKey.publicKey
        val sig = privKey.schnorrSignWithNonce(data, nonce)
        assert(sig.rx == nonce.schnorrNonce)

        val sigPoint = pubKey.schnorrComputePoint(data, sig.rx)
        assert(sigPoint == sig.sig.toPrivateKey.publicKey)
    }
  }

  /** Schnorr signatures have the property that if two messages are signed with the same key
    * and nonce, then they are leaked:
    *
    * sig1 = nonce + message1*privKey
    * sig2 = nonce + message2*privKey
    *
    * => sig1 - sig2 = (message1 - message2)*privKey
    * => privKey = (sig1 - sig2) * inverse(message1 - message2)
    */
  it must "leak keys if two messages are signed" in {
    forAll(CryptoGenerators.nonZeroPrivKey,
           CryptoGenerators.nonZeroPrivKey,
           NumberGenerator.bytevector(32),
           NumberGenerator.bytevector(32)) {
      case (privKey, nonce, message1, message2) =>
        // This will only work if we sign two different messages
        assert(message1 != message2)

        // Sign both messages using the same privKey and nonce
        val sig1 = privKey.schnorrSignWithNonce(message1, nonce)
        val sig2 = privKey.schnorrSignWithNonce(message2, nonce)

        // s1 = nonce + e1*privKey
        val s1 = sig1.sig
        // s2 = nonce + e2*privKey
        val s2 = sig2.sig

        // When signing a message you actually sign SHA256_challenge(Rx || pubKey || message)
        val bytesToHash1 = sig1.rx.bytes ++ privKey.schnorrPublicKey.bytes ++ message1
        val e1Bytes = CryptoUtil.sha256SchnorrChallenge(bytesToHash1).bytes

        val bytesToHash2 = sig2.rx.bytes ++ privKey.schnorrPublicKey.bytes ++ message2
        val e2Bytes = CryptoUtil.sha256SchnorrChallenge(bytesToHash2).bytes

        val e1 = NumberUtil.uintToFieldElement(e1Bytes)
        val e2 = NumberUtil.uintToFieldElement(e2Bytes)

        val k = nonce.nonceKey.fieldElement
        val x = privKey.schnorrKey.fieldElement

        // Test that we have correctly computed the components
        assert(k.add(e1.multiply(x)) == s1)
        assert(k.add(e2.multiply(x)) == s2)

        // Note that all of s1, s2, e1, and e2 are public information:
        // s1 - s2 = nonce + e1*privKey - (nonce + e2*privKey) = privKey*(e1-e2)
        // => privKey = (s1 - s2) * modInverse(e1 - e2)
        val privNum = s1.subtract(s2).multInv(e1.subtract(e2))

        // Assert that we've correctly recovered the private key form public info
        assert(privNum == x)
    }
  }
}
