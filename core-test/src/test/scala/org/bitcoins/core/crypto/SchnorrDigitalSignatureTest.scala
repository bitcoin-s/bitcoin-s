package org.bitcoins.core.crypto

import org.bitcoins.core.util.{CryptoUtil, NumberUtil}
import org.bitcoins.testkit.core.gen.{CryptoGenerators, NumberGenerator}
import org.bitcoins.testkit.util.BitcoinSUnitTest

class SchnorrDigitalSignatureTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "SchnorrDigitalSignature"

  it must "have serialization symmetry" in {
    forAll(NumberGenerator.bytevector(64)) { bytes =>
      val sig = SchnorrDigitalSignature(bytes)
      assert(sig.bytes == bytes)
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
        assert(sigPoint == ECPrivateKey(sig.sig).publicKey)
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
        assert(sigPoint == ECPrivateKey(sig.sig).publicKey)
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
    // The order of the secp256k1 curve (and thus the modulus of the field elements)
    val N = BigInt(CryptoParams.params.getN)

    /** Returns num % N as a positive integer */
    def modN(num: BigInt): BigInt = {
      if (num < 0) {
        (num % N) + N
      } else {
        num % N
      }
    }

    /** Computes the inverse (mod M) of the input using the Euclidean Algorithm (log time)
      * Cribbed from [[https://www.geeksforgeeks.org/multiplicative-inverse-under-modulo-m/]]
      */
    def modInverse(aInit: BigInt): BigInt = {
      var a = modN(aInit)
      var m = N
      var m0 = m
      var y = BigInt(0)
      var x = BigInt(1)

      if (m == 1)
        return 0

      while (a > 1) {
        // q is quotient
        val q = a / m

        var t = m

        // m is remainder now, process
        // same as Euclid's algo
        m = a % m
        a = t
        t = y

        // Update x and y
        y = x - q * y
        x = t
      }

      if (x < 0) {
        x += m0
      }

      modN(x)
    }

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
        val s1 = NumberUtil.toUnsignedInt(sig1.sig)
        // s2 = nonce + e2*privKey
        val s2 = NumberUtil.toUnsignedInt(sig2.sig)

        // When signing a message you actually sign SHA256_challenge(Rx || pubKey || message)
        val e1Bytes =
          CryptoUtil
            .taggedSha256(
              sig1.rx.bytes ++ privKey.schnorrPublicKey.bytes ++ message1,
              "BIP340/challenge")
            .bytes
        val e2Bytes =
          CryptoUtil
            .taggedSha256(
              sig2.rx.bytes ++ privKey.schnorrPublicKey.bytes ++ message2,
              "BIP340/challenge")
            .bytes

        val e1 = NumberUtil.toUnsignedInt(e1Bytes)
        val e2 = NumberUtil.toUnsignedInt(e2Bytes)

        val k = NumberUtil.toUnsignedInt(nonce.nonceKey.bytes)
        val x = NumberUtil.toUnsignedInt(privKey.schnorrKey.bytes)

        // This tests that modInverse is working
        assert(modN(modInverse(e1 - e2) * modN(e1 - e2)) == BigInt(1))

        // Test that we have correctly computed the components
        assert(modN(k + (e1 * x)) == s1)
        assert(modN(k + (e2 * x)) == s2)

        // Note that all of s1, s2, e1, and e2 are public information:
        // s1 - s2 = nonce + e1*privKey - (nonce + e2*privKey) = privKey*(e1-e2)
        // => privKey = (s1 - s2) * modInverse(e1 - e2)
        val privNum = modN(modN(s1 - s2) * modInverse(e1 - e2))

        // Assert that we've correctly recovered the private key form public info
        assert(privNum == x)
    }
  }
}
