package org.bitcoins.crypto

import org.bitcoins.testkitcore.gen.{CryptoGenerators, NumberGenerator}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalacheck.Gen
import org.scalatest.{Outcome, Succeeded}

class BouncyCastleSecp256k1Test extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "CryptoLibraries"

  override def withFixture(test: NoArgTest): Outcome = {
    CryptoContext.default match {
      case CryptoContext.LibSecp256k1 => super.withFixture(test)
      case CryptoContext.BouncyCastle | CryptoContext.BCrypto =>
        logger.warn(s"Test ${test.name} skipped as Secp256k1 is not available.")
        Succeeded
    }
  }

  it must "add private keys the same" in {
    forAll(CryptoGenerators.privateKey, CryptoGenerators.privateKey) {
      case (priv1, priv2) =>
        assert(
          BouncycastleCryptoRuntime
            .add(priv1, priv2) == LibSecp256k1CryptoRuntime.add(priv1, priv2))
    }
  }

  it must "add public keys the same" in {
    forAll(CryptoGenerators.publicKey, CryptoGenerators.privateKey) {
      case (pubKey, privKey) =>
        val sumKeyExpected =
          LibSecp256k1CryptoRuntime.pubKeyTweakAdd(pubKey, privKey)
        val sumKey =
          BouncycastleCryptoRuntime.pubKeyTweakAdd(pubKey, privKey)

        assert(sumKey == sumKeyExpected)
    }
  }

  it must "multiply keys the same" in {
    forAll(CryptoGenerators.publicKey, CryptoGenerators.fieldElement) {
      case (pubKey, tweak) =>
        assert(
          LibSecp256k1CryptoRuntime.tweakMultiply(pubKey, tweak) ==
            BouncycastleCryptoRuntime.tweakMultiply(pubKey, tweak))
    }
  }

  it must "validate keys the same" in {
    val keyOrGarbageGen =
      Gen.oneOf(CryptoGenerators.publicKey.map(_.bytes),
                NumberGenerator.bytevector(33))
    forAll(keyOrGarbageGen) { bytes =>
      assert(
        LibSecp256k1CryptoRuntime.isValidPubKey(bytes) ==
          BouncycastleCryptoRuntime.isValidPubKey(bytes)
      )
    }
  }

  it must "decompress keys the same" in {
    forAll(CryptoGenerators.publicKey) { pubKey =>
      assert(
        LibSecp256k1CryptoRuntime.decompressed(pubKey) ==
          BouncycastleCryptoRuntime.decompressed(pubKey)
      )
    }
  }

  it must "compute public keys the same" in {
    forAll(CryptoGenerators.privateKey) { privKey =>
      assert(
        LibSecp256k1CryptoRuntime.publicKey(privKey) ==
          BouncycastleCryptoRuntime.publicKey(privKey)
      )
    }
  }

  it must "compute signatures the same" in {
    forAll(CryptoGenerators.privateKey, NumberGenerator.bytevector(32)) {
      case (privKey, bytes) =>
        assert(
          LibSecp256k1CryptoRuntime.sign(
            privKey,
            bytes) == BouncycastleCryptoRuntime.sign(privKey, bytes)
        )
    }
  }

  it must "compute signatures with entropy the same" in {
    forAll(CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32),
           NumberGenerator.bytevector(32)) { case (privKey, bytes, entropy) =>
      assert(
        LibSecp256k1CryptoRuntime.signWithEntropy(
          privKey,
          bytes,
          entropy) == BouncycastleCryptoRuntime.signWithEntropy(privKey,
                                                                bytes,
                                                                entropy)
      )
    }
  }

  it must "verify signatures the same" in {
    forAll(CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32),
           CryptoGenerators.digitalSignature) { case (privKey, bytes, badSig) =>
      val sig = privKey.sign(bytes)
      val pubKey = privKey.publicKey
      assert(
        LibSecp256k1CryptoRuntime.verify(
          pubKey,
          bytes,
          sig) == BouncycastleCryptoRuntime.verify(pubKey, bytes, sig)
      )
      assert(
        LibSecp256k1CryptoRuntime.verify(
          pubKey,
          bytes,
          badSig) == BouncycastleCryptoRuntime.verify(pubKey, bytes, badSig)
      )
    }
  }

  /*
  it must "compute schnorr signatures the same" in {
    forAll(CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32),
           NumberGenerator.bytevector(32)) {
      case (privKey, bytes, auxRand) =>
        assert(
          privKey.schnorrSign(bytes, auxRand, context = BouncyCastle) == privKey
            .schnorrSign(bytes, auxRand, context = LibSecp256k1))
    }
  }

  it must "compute schnorr signature for fixed nonce the same" in {
    forAll(CryptoGenerators.privateKey,
           CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32)) {
      case (privKey, nonceKey, bytes) =>
        val sigBC = privKey
          .schnorrSignWithNonce(bytes, nonceKey, context = BouncyCastle)
        val sigSecP = privKey
          .schnorrSignWithNonce(bytes, nonceKey, context = LibSecp256k1)
        assert(sigBC.bytes == sigSecP.bytes)
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
          pubKey.verify(bytes, sig, context = BouncyCastle) == pubKey
            .verify(bytes, sig, context = LibSecp256k1))
        assert(
          pubKey.verify(bytes, badSig, context = BouncyCastle) == pubKey
            .verify(bytes, badSig, context = LibSecp256k1))
    }
  }

  it must "compute schnorr signature points the same" in {
    forAll(CryptoGenerators.schnorrPublicKey,
           CryptoGenerators.schnorrNonce,
           NumberGenerator.bytevector(32)) {
      case (pubKey, nonce, bytes) =>
        val bouncyCastleSigPoint =
          pubKey.computeSigPoint(bytes,
                                 nonce,
                                 compressed = true,
                                 context = BouncyCastle)

        val secpSigPoint = pubKey.computeSigPoint(bytes,
                                                  nonce,
                                                  compressed = true,
                                                  context = LibSecp256k1)

        assert(bouncyCastleSigPoint == secpSigPoint)
    }
  }
   */
}
