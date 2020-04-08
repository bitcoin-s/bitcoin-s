package org.bitcoins.core.crypto

import java.math.BigInteger

import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.testkit.core.gen.{CryptoGenerators, NumberGenerator}
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.ByteVector

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
        val privKeyForUse = if (privKey.publicKey.hex.take(2) == "03") {
          val negPrivKeyNum = CryptoParams.curve.getN
            .subtract(new BigInteger(1, privKey.bytes.toArray))
          val negPrivKey = ECPrivateKey(ByteVector(negPrivKeyNum.toByteArray))

          assert(
            privKey.publicKey.bytes.tail == negPrivKey.publicKey.bytes.tail)

          negPrivKey
        } else privKey

        val randHash = CryptoUtil.taggedSha256(auxRand, "BIP340/aux").bytes
        val maskedKey = randHash.xor(privKeyForUse.bytes)

        val nonceHash = CryptoUtil.taggedSha256(
          maskedKey ++ privKey.publicKey.bytes.tail ++ bytes,
          "BIP340/nonce")
        val nonce = ECPrivateKey(nonceHash.bytes)

        val sig1 = privKey.schnorrSign(bytes, auxRand)
        val sig2 = privKey.schnorrSignWithNonce(bytes, nonce)

        assert(sig1 == sig2)
    }
  }
}
