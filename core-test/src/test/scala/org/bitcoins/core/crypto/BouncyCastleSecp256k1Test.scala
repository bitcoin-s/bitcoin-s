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
        assert(priv1.addWithBouncyCastle(priv2) == priv1.addWithSecp(priv2))
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
    forAll(CryptoGenerators.publicKey, CryptoGenerators.fieldElement) {
      case (pubKey, tweak) =>
        assert(
          pubKey.tweakMultiplyWithSecp(tweak) == pubKey
            .tweakMultiplyWithBouncyCastle(tweak))
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
          privKey.schnorrSignWithBouncyCastle(bytes, auxRand) == privKey
            .schnorrSignWithSecp(bytes, auxRand))
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
          pubKey.verifyWithBouncyCastle(bytes, sig) == pubKey
            .verifyWithSecp(bytes, sig))
        assert(
          pubKey.verifyWithBouncyCastle(bytes, badSig) == pubKey
            .verifyWithSecp(bytes, badSig))
    }
  }

  it must "compute schnorr signature points the same" in {
    forAll(CryptoGenerators.schnorrPublicKey,
           CryptoGenerators.schnorrNonce,
           NumberGenerator.bytevector(32)) {
      case (pubKey, nonce, bytes) =>
        assert(
          pubKey.computeSigPointWithBouncyCastle(bytes, nonce) == pubKey
            .computeSigPointWithSecp(bytes, nonce))
    }
  }
}
