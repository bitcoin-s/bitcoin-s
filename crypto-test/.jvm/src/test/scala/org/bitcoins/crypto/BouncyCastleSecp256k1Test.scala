package org.bitcoins.crypto

import org.scalacheck.Gen
import org.scalatest.{Outcome, Succeeded}

class BouncyCastleSecp256k1Test extends BitcoinSCryptoTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "CryptoLibraries"

  override def withFixture(test: NoArgTest): Outcome = {
    CryptoContext.default match {
      case CryptoContext.LibSecp256k1 => super.withFixture(test)
      case CryptoContext.BouncyCastle | CryptoContext.BCrypto =>
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

  it must "compute schnorr signatures the same" in {
    forAll(CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32),
           NumberGenerator.bytevector(32)) { case (privKey, bytes, auxRand) =>
      assert(
        LibSecp256k1CryptoRuntime
          .schnorrSign(bytes, privKey, auxRand) == BouncycastleCryptoRuntime
          .schnorrSign(bytes, privKey, auxRand))
    }
  }

  it must "compute schnorr signature for fixed nonce the same" in {
    forAll(CryptoGenerators.privateKey,
           CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32)) { case (privKey, nonceKey, bytes) =>
      assert(
        LibSecp256k1CryptoRuntime.schnorrSignWithNonce(
          bytes,
          privKey,
          nonceKey) == BouncycastleCryptoRuntime.schnorrSignWithNonce(bytes,
                                                                      privKey,
                                                                      nonceKey))
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
          LibSecp256k1CryptoRuntime.schnorrVerify(
            bytes,
            pubKey,
            sig) == BouncycastleCryptoRuntime.schnorrVerify(bytes, pubKey, sig))
        assert(
          LibSecp256k1CryptoRuntime
            .schnorrVerify(bytes, pubKey, badSig) == BouncycastleCryptoRuntime
            .schnorrVerify(bytes, pubKey, badSig))
    }
  }

  it must "compute schnorr signature points the same" in {
    forAll(CryptoGenerators.schnorrPublicKey,
           CryptoGenerators.schnorrNonce,
           NumberGenerator.bytevector(32)) { case (pubKey, nonce, bytes) =>
      assert(
        LibSecp256k1CryptoRuntime.schnorrComputeSigPoint(
          bytes,
          nonce,
          pubKey,
          compressed = true) == BouncycastleCryptoRuntime
          .schnorrComputeSigPoint(bytes, nonce, pubKey, compressed = true))
    }
  }

  it must "compute adaptor signatures the same" in {
    forAll(CryptoGenerators.privateKey,
           CryptoGenerators.publicKey,
           NumberGenerator.bytevector(32),
           NumberGenerator.bytevector(32)) {
      case (privKey, adaptor, msg, auxRand) =>
        assert(
          LibSecp256k1CryptoRuntime.adaptorSign(
            privKey,
            adaptor,
            msg,
            auxRand) == BouncycastleCryptoRuntime
            .adaptorSign(privKey, adaptor, msg, auxRand))
    }
  }

  it must "verify adaptor signatures the same" in {
    forAll(CryptoGenerators.privateKey,
           CryptoGenerators.publicKey,
           NumberGenerator.bytevector(32),
           CryptoGenerators.adaptorSignature) {
      case (privKey, adaptor, msg, badSig) =>
        val sig = privKey.adaptorSign(adaptor, msg)
        val pubKey = privKey.publicKey

        assert(LibSecp256k1CryptoRuntime
          .adaptorVerify(sig, pubKey, msg, adaptor) == BouncycastleCryptoRuntime
          .adaptorVerify(sig, pubKey, msg, adaptor))
        assert(
          LibSecp256k1CryptoRuntime.adaptorVerify(
            badSig,
            pubKey,
            msg,
            adaptor) == BouncycastleCryptoRuntime
            .adaptorVerify(badSig, pubKey, msg, adaptor))
    }
  }

  it must "complete adaptor signatures the same" in {
    forAll(CryptoGenerators.privateKey, CryptoGenerators.adaptorSignature) {
      case (adaptorSecret, adaptorSig) =>
        assert(
          LibSecp256k1CryptoRuntime.adaptorComplete(
            adaptorSecret,
            adaptorSig) == BouncycastleCryptoRuntime
            .adaptorComplete(adaptorSecret, adaptorSig))
    }
  }

  it must "extract adaptor secrets the same" in {
    forAll(CryptoGenerators.adaptorSignatureWithDecryptedSignatureAndAdaptor) {
      case (adaptorSig, sig, adaptor) =>
        assert(
          LibSecp256k1CryptoRuntime.extractAdaptorSecret(sig,
                                                         adaptorSig,
                                                         adaptor) ==
            BouncycastleCryptoRuntime.extractAdaptorSecret(sig,
                                                           adaptorSig,
                                                           adaptor))
    }
  }
}
