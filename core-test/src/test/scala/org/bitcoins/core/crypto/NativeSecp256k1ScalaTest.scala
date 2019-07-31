package org.bitcoins.core.crypto

import org.bitcoin.NativeSecp256k1
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.testkit.core.gen.NumberGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.ByteVector

class NativeSecp256k1ScalaTest extends BitcoinSUnitTest {
  behavior of "NativeSecp256k1"

  it must "act like a normal schnorrSign if the normal nonce is specified" in {
    forAll(
      NumberGenerator.bytevector(32).filter(_.toArray.exists(_ != 0.toByte)),
      NumberGenerator.bytevector(32)) {
      case (privKeyBytes, message) =>
        val privKey = ECPrivateKey.fromBytes(privKeyBytes)

        // This is the nonce specified in bip-schnorr
        val nonce =
          CryptoUtil.sha256(privKey.bytes ++ message).bytes

        assert(nonce.toArray.exists(_ != 0.toByte))

        val sigWithNonce =
          NativeSecp256k1.schnorrSignWithNonce(message.toArray,
                                               privKeyBytes.toArray,
                                               nonce.toArray)

        val sig =
          NativeSecp256k1.schnorrSign(message.toArray, privKeyBytes.toArray)

        assert(ByteVector(sigWithNonce) == ByteVector(sig))
    }
  }

  it must "correctly compute a Schnorr public key given an R value" in {
    forAll(
      NumberGenerator.bytevector(32).filter(_.toArray.exists(_ != 0.toByte)),
      NumberGenerator.bytevector(32)) {
      case (privKeyBytes, message) =>
        val privKey: ECPrivateKey =
          ECPrivateKey.fromBytes(privKeyBytes)
        val publicKey: ECPublicKey = privKey.publicKey
        val messageArr: Array[Byte] = message.toArray

        // This is the nonce specified in bip-schnorr
        val nonce =
          CryptoUtil.sha256(privKey.bytes ++ message).bytes

        assert(nonce.toArray.exists(_ != 0.toByte))

        val rBytes = NativeSecp256k1.schnorrPublicNonce(nonce.toArray)

        val sig: Array[Byte] =
          NativeSecp256k1.schnorrSign(messageArr, privKey.bytes.toArray)

        val (rKey, s) = sig.splitAt(32)

        assert(ByteVector(rBytes.tail) == ByteVector(rKey))

        val sPub: Array[Byte] =
          NativeSecp256k1.computeSchnorrPubKey(messageArr,
                                               rKey.+:(rBytes.head),
                                               publicKey.bytes.toArray)

        val realSPriv = ECPrivateKey.fromBytes(ByteVector(s))
        val realSPub = realSPriv.publicKey

        assert(ECPublicKey.fromBytes(ByteVector(sPub)) == realSPub)
    }
  }
}
