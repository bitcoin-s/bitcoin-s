package org.bitcoins.core.crypto

import org.bitcoin.NativeSecp256k1
import org.bitcoins.core.util.{CryptoUtil, NumberUtil}
import org.bitcoins.testkit.core.gen.{CryptoGenerators, NumberGenerator}
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.ByteVector

class SchnorrTest extends BitcoinSUnitTest {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "NativeSecp256k1"

  it must "act like a normal schnorrSign if the normal nonce is specified" in {
    forAll(CryptoGenerators.nonZeroPrivKey, NumberGenerator.bytevector(32)) {
      case (privKey, message) =>
        val nonce = SchnorrNonce.fromBipSchnorr(privKey, message)

        assert(nonce.bytes.toArray.exists(_ != 0.toByte))

        val sigWithNonce =
          NativeSecp256k1.schnorrSignWithNonce(message.toArray,
                                               privKey.bytes.toArray,
                                               nonce.bytes.toArray)

        assert(
          SchnorrDigitalSignature(ByteVector(sigWithNonce)) ==
            Schnorr.signWithNonce(message, privKey, nonce)
        )

        val sig =
          NativeSecp256k1.schnorrSign(message.toArray, privKey.bytes.toArray)

        assert(
          SchnorrDigitalSignature(ByteVector(sig)) ==
            Schnorr.sign(message, privKey)
        )

        assert(ByteVector(sigWithNonce) == ByteVector(sig))
    }
  }

  it must "correctly compute a Schnorr public key given an R value" in {
    forAll(CryptoGenerators.nonZeroPrivKey, NumberGenerator.bytevector(32)) {
      case (privKey, message) =>
        val publicKey: ECPublicKey = privKey.publicKey
        val messageArr: Array[Byte] = message.toArray

        val nonce = SchnorrNonce.fromBipSchnorr(privKey, message)

        assert(nonce.bytes.toArray.exists(_ != 0.toByte))

        val rBytes = nonce.publicKey.bytes

        val sig: Array[Byte] =
          NativeSecp256k1.schnorrSign(messageArr, privKey.bytes.toArray)

        val (rKey, s) = sig.splitAt(32)

        assert(rBytes.tail == ByteVector(rKey))

        val sPub: Array[Byte] =
          NativeSecp256k1.computeSchnorrPubKey(messageArr,
                                               rBytes.toArray,
                                               publicKey.bytes.toArray)
        val sPubKey: ECPublicKey = ECPublicKey.fromBytes(ByteVector(sPub))

        assert(
          sPubKey == Schnorr.computePubKey(message, nonce.publicKey, publicKey))

        val realSPriv = ECPrivateKey.fromBytes(ByteVector(s))
        val realSPub = realSPriv.publicKey

        assert(sPubKey == realSPub)
    }
  }

  it should "leak keys if two messages are signed" in {
    // The order of the secp256k1 curve
    val M = BigInt(
      "115792089237316195423570985008687907852837564279074904382605163141518161494337")

    /** Returns num % M as a positive integer */
    def modM(num: BigInt): BigInt = {
      if (num < 0) {
        (num % M) + M
      } else {
        num % M
      }
    }

    /** Cribbed from [[https://www.geeksforgeeks.org/multiplicative-inverse-under-modulo-m/]] */
    def modInverse(aInit: BigInt): BigInt = {
      var a = modM(aInit)
      var m = M
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

      modM(x)
    }

    forAll(CryptoGenerators.nonZeroPrivKey,
           CryptoGenerators.schnorrNonce,
           NumberGenerator.bytevector(32),
           NumberGenerator.bytevector(32)) {
      case (privKey, nonce, message1, message2) =>
        assert(message1 != message2)

        val sig1 = Schnorr.signWithNonce(message1, privKey, nonce)
        val sig2 = Schnorr.signWithNonce(message2, privKey, nonce)

        // s1 = nonce + e1*privKey
        val s1 = NumberUtil.toUnsignedInt(sig1.s)
        // s2 = nonce + e2*privKey
        val s2 = NumberUtil.toUnsignedInt(sig2.s)

        val e1Bytes =
          CryptoUtil
            .sha256(sig1.rx ++ privKey.publicKey.bytes ++ message1)
            .bytes
        val e2Bytes =
          CryptoUtil
            .sha256(sig2.rx ++ privKey.publicKey.bytes ++ message2)
            .bytes

        val e1 = NumberUtil.toUnsignedInt(e1Bytes)
        val e2 = NumberUtil.toUnsignedInt(e2Bytes)

        // This tests that modInverse is working
        assert(modM(modInverse(e1 - e2) * modM(e1 - e2)) == BigInt(1))

        // Note that all of s1, s2, e1, and e2 are public information:
        // s1 - s2 = nonce + e1*privKey - (nonce + e2*privKey) = privKey*(e1-e2)
        // => privKey = (s1 - s2) * modInverse(e1 - e2)
        val privNum = modM(modM(s1 - s2) * modInverse(e1 - e2))

        assert(privNum == NumberUtil.toUnsignedInt(privKey.bytes))
    }
  }
}
