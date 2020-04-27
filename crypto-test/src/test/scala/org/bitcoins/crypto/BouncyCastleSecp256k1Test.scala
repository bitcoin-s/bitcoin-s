package org.bitcoins.crypto

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

  it must "validate keys the same" in {
    val keyOrGarbageGen = Gen.oneOf(CryptoGenerators.publicKey.map(_.bytes),
                                    NumberGenerator.bytevector(33))
    forAll(keyOrGarbageGen) { bytes =>
      assert(
        ECPublicKey.isFullyValid(bytes, useSecp = false) ==
          ECPublicKey.isFullyValid(bytes, useSecp = true)
      )
    }
  }

  it must "decompress keys the same" in {
    forAll(CryptoGenerators.publicKey) { pubKey =>
      assert(
        pubKey.decompressed(useSecp = false) == pubKey.decompressed(
          useSecp = true))
    }
  }

  it must "compute public keys the same" in {
    forAll(CryptoGenerators.privateKey) { privKey =>
      assert(
        privKey.publicKey(useSecp = false) == privKey.publicKey(useSecp = true))
    }
  }

  it must "compute signatures the same" in {
    forAll(CryptoGenerators.privateKey, NumberGenerator.bytevector(32)) {
      case (privKey, bytes) =>
        assert(
          privKey.sign(bytes, useSecp = false) == privKey.sign(bytes,
                                                               useSecp = true))
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
          pubKey.verify(bytes, sig, useSecp = false) == pubKey
            .verify(bytes, sig, useSecp = true))
        assert(
          pubKey.verify(bytes, badSig, useSecp = false) == pubKey
            .verify(bytes, badSig, useSecp = true))
    }
  }
}
