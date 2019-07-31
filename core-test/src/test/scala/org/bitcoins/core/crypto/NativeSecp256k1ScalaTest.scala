package org.bitcoins.core.crypto

import org.bitcoin.NativeSecp256k1
import org.bitcoins.testkit.core.gen.NumberGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.ByteVector

class NativeSecp256k1ScalaTest extends BitcoinSUnitTest {
  behavior of "NativeSecp256k1"

  it must "correctly compute a Schnorr public key given an R value" in {
    forAll(NumberGenerator.bytes(32), NumberGenerator.bytes(32)) {
      case (privKeyBytes, messageBytes) =>
        val privKey: ECPrivateKey =
          ECPrivateKey.fromBytes(ByteVector(privKeyBytes))
        val publicKey: ECPublicKey = privKey.publicKey
        val message: Array[Byte] = messageBytes.toArray

        val sig: Array[Byte] =
          NativeSecp256k1.schnorrSign(message, privKey.bytes.toArray)

        val (rKey, s) = sig.splitAt(32)

        val sPub1: Array[Byte] =
          NativeSecp256k1.computeSchnorrPubKey(message,
                                               rKey.+:(2.toByte),
                                               publicKey.bytes.toArray)

        val sPub2: Array[Byte] =
          NativeSecp256k1.computeSchnorrPubKey(message,
                                               rKey.+:(3.toByte),
                                               publicKey.bytes.toArray)

        val realSPriv = ECPrivateKey.fromBytes(ByteVector(s))
        val realSPub = realSPriv.publicKey

        assert(
          ECPublicKey.fromBytes(ByteVector(sPub1)) == realSPub ||
            ECPublicKey.fromBytes(ByteVector(sPub2)) == realSPub
        )
    }
  }
}
