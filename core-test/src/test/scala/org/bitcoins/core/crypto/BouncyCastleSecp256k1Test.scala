package org.bitcoins.core.crypto

import org.bitcoin.{NativeSecp256k1, Secp256k1Context}
import org.bitcoins.testkit.core.gen.{CryptoGenerators, NumberGenerator}
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalacheck.Gen
import org.scalatest.{Outcome, Succeeded}
import scodec.bits.ByteVector

class BouncyCastleSecp256k1Test extends BitcoinSUnitTest {

  behavior of "CryptoLibraries"

  override def withFixture(test: NoArgTest): Outcome = {
    if (Secp256k1Context.isEnabled) {
      super.withFixture(test)
    } else {
      logger.warn(s"Test ${test.name} skipped as Secp256k1 is not available.")
      Succeeded
    }
  }

  it must "add private keys the same" in {
    forAll(CryptoGenerators.privateKey, CryptoGenerators.privateKey) {
      case (priv1, priv2) =>
        val sumWithBouncyCastle =
          BouncyCastleUtil.addNumbers(priv1.bytes, priv2.bytes)
        val sumWithSecp = NativeSecp256k1.privKeyTweakAdd(priv1.bytes.toArray,
                                                          priv2.bytes.toArray)

        val sumKeyWithBouncyCastle =
          ECPrivateKey(ByteVector(sumWithBouncyCastle.toByteArray))
        val sumKeyWithSecp = ECPrivateKey(ByteVector(sumWithSecp))

        assert(sumKeyWithBouncyCastle == sumKeyWithSecp)
    }
  }

  it must "add public keys the same" in {
    forAll(CryptoGenerators.publicKey, CryptoGenerators.privateKey) {
      case (pubKey, privKey) =>
        val sumKeyBytes =
          NativeSecp256k1.pubKeyTweakAdd(pubKey.bytes.toArray,
                                         privKey.bytes.toArray,
                                         true)
        val sumKeyExpected = ECPublicKey.fromBytes(ByteVector(sumKeyBytes))
        val sumKey = pubKey.addWithBouncyCastle(privKey.publicKey)

        assert(sumKey == sumKeyExpected)
    }
  }

  it must "multiply keys the same" in {
    forAll(CryptoGenerators.publicKey, CryptoGenerators.privateKey) {
      case (pubKey, tweak) =>
        val multKeyBytes = NativeSecp256k1.pubKeyTweakMul(pubKey.bytes.toArray,
                                                          tweak.bytes.toArray,
                                                          pubKey.isCompressed)
        val multKeyExpected = ECPublicKey.fromBytes(ByteVector(multKeyBytes))
        val multKey = BouncyCastleUtil.pubKeyTweakMul(pubKey, tweak.bytes)

        assert(multKey == multKeyExpected)
    }
  }

  it must "validate keys the same" in {
    val keyOrGarbageGen = Gen.oneOf(CryptoGenerators.publicKey.map(_.bytes),
                                    NumberGenerator.bytevector(33))
    forAll(keyOrGarbageGen) { bytes =>
      assert(
        ECPublicKey.isFullyValidWithBouncyCastle(bytes) ==
          ECPublicKey.isFullyValidWithSecp(bytes)
      )
    }
  }

  it must "decompress keys the same" in {
    forAll(CryptoGenerators.publicKey) { pubKey =>
      assert(pubKey.decompressedWithBouncyCastle == pubKey.decompressedWithSecp)
    }
  }

  it must "compute public keys the same" in {
    forAll(CryptoGenerators.privateKey) { privKey =>
      assert(privKey.publicKeyWithBouncyCastle == privKey.publicKeyWithSecp)
    }
  }

  it must "compute signatures the same" in {
    forAll(CryptoGenerators.privateKey, NumberGenerator.bytevector(32)) {
      case (privKey, bytes) =>
        assert(
          privKey.signWithBouncyCastle(bytes) == privKey.signWithSecp(bytes))
    }
  }

  it must "verify signatures the same" in {
    forAll(CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32),
           CryptoGenerators.digitalSignature) {
      case (privKey, bytes, badSig) =>
        val sig = privKey.sign(bytes)
        val pubKey = privKey.publicKey
        assert(
          pubKey.verifyWithBouncyCastle(bytes, sig) == pubKey
            .verifyWithSecp(bytes, sig))
        assert(
          pubKey.verifyWithBouncyCastle(bytes, badSig) == pubKey
            .verifyWithSecp(bytes, badSig))
    }
  }

  it must "compute schnorr signatures the same" in {
    forAll(CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32),
           NumberGenerator.bytevector(32)) {
      case (privKey, bytes, auxRand) =>
        assert(
          BouncyCastleUtil.schnorrSign(bytes, privKey, auxRand) == privKey
            .schnorrSign(bytes, auxRand))
    }
  }

  it must "validate schnorr signatures the same" in {
    forAll(CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32),
           CryptoGenerators.schnorrDigitalSignature) {
      case (privKey, bytes, badSig) =>
        val sig = privKey.schnorrSign(bytes)
        val pubKey = privKey.schnorrPublicKey
        assert(
          BouncyCastleUtil
            .schnorrVerify(bytes, privKey.schnorrPublicKey, sig) == pubKey
            .verify(bytes, sig))
        assert(
          BouncyCastleUtil.schnorrVerify(bytes,
                                         privKey.schnorrPublicKey,
                                         badSig) == pubKey
            .verify(bytes, badSig))
    }
  }

  it must "compute schnorr signature points the same" in {
    forAll(CryptoGenerators.publicKey,
           CryptoGenerators.schnorrNonce,
           NumberGenerator.bytevector(32)) {
      case (pubKey, nonce, bytes) =>
        assert(
          BouncyCastleUtil.schnorrComputeSigPoint(
            bytes,
            nonce,
            pubKey.schnorrPublicKey,
            pubKey.isCompressed) == pubKey.schnorrComputePoint(bytes, nonce))
    }
  }
}
