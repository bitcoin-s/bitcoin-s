package org.bitcoins.crypto

import org.scalatest.{Assertion, Outcome, Succeeded}
import scodec.bits.ByteVector

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

  def testCompatibility[T](func: CryptoRuntime => T): Assertion = {
    assert(func(BouncycastleCryptoRuntime) == func(LibSecp256k1CryptoRuntime))
  }

  it must "add private keys the same" in {
    forAll(CryptoGenerators.privateKey, CryptoGenerators.privateKey) {
      case (priv1, priv2) => testCompatibility(_.add(priv1, priv2))
    }
  }

  it must "add public keys the same" in {
    forAll(CryptoGenerators.publicKey, CryptoGenerators.privateKey) {
      case (pubKey, privKey) =>
        testCompatibility(_.pubKeyTweakAdd(pubKey, privKey))
    }
  }

  it must "multiply keys the same" in {
    forAll(CryptoGenerators.publicKey, CryptoGenerators.fieldElement) {
      case (pubKey, tweak) => testCompatibility(_.tweakMultiply(pubKey, tweak))
    }
  }

  it must "decompress keys the same" in {
    forAll(CryptoGenerators.publicKey) { pubKey =>
      testCompatibility(_.decompressed(pubKey))
    }
  }

  it must "decompress edge case keys the same" in {
    val pubKeyBytes = ByteVector.fromValidHex(
      "03fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2c")
    testCompatibility(_.decompressed(pubKeyBytes))
  }

  it must "compute public keys the same" in {
    forAll(CryptoGenerators.privateKey) { privKey =>
      testCompatibility(_.publicKey(privKey))
    }
  }

  it must "compute signatures the same" in {
    forAll(CryptoGenerators.privateKey, NumberGenerator.bytevector(32)) {
      case (privKey, bytes) => testCompatibility(_.sign(privKey, bytes))
    }
  }

  it must "compute signatures with entropy the same" in {
    forAll(CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32),
           NumberGenerator.bytevector(32)) { case (privKey, bytes, entropy) =>
      testCompatibility(_.signWithEntropy(privKey, bytes, entropy))
    }
  }

  it must "verify signatures the same" in {
    forAll(CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32),
           CryptoGenerators.digitalSignature) { case (privKey, bytes, badSig) =>
      val sig = privKey.sign(bytes)
      val pubKey = privKey.publicKey

      testCompatibility(_.verify(pubKey, bytes, sig))
      testCompatibility(_.verify(pubKey, bytes, badSig))
    }
  }

  it must "compute schnorr signatures the same" in {
    forAll(CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32),
           NumberGenerator.bytevector(32)) { case (privKey, bytes, auxRand) =>
      testCompatibility(_.schnorrSign(bytes, privKey, auxRand))
    }
  }

  it must "compute schnorr signature for fixed nonce the same" in {
    forAll(CryptoGenerators.privateKey,
           CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32)) { case (privKey, nonceKey, bytes) =>
      testCompatibility(_.schnorrSignWithNonce(bytes, privKey, nonceKey))
    }
  }

  it must "validate schnorr signatures the same" in {
    forAll(CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32),
           CryptoGenerators.schnorrDigitalSignature) {
      case (privKey, bytes, badSig) =>
        val sig = privKey.schnorrSign(bytes)
        val pubKey = privKey.schnorrPublicKey

        testCompatibility(_.schnorrVerify(bytes, pubKey, sig))
        testCompatibility(_.schnorrVerify(bytes, pubKey, badSig))
    }
  }

  it must "compute schnorr signature points the same" in {
    forAll(CryptoGenerators.schnorrPublicKey,
           CryptoGenerators.schnorrNonce,
           NumberGenerator.bytevector(32)) { case (pubKey, nonce, bytes) =>
      testCompatibility(
        _.schnorrComputeSigPoint(bytes, nonce, pubKey, compressed = true))
    }
  }

  it must "compute adaptor signatures the same" in {
    forAll(CryptoGenerators.privateKey,
           CryptoGenerators.publicKey,
           NumberGenerator.bytevector(32),
           NumberGenerator.bytevector(32)) {
      case (privKey, adaptor, msg, auxRand) =>
        testCompatibility(_.adaptorSign(privKey, adaptor, msg, auxRand))
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

        testCompatibility(_.adaptorVerify(sig, pubKey, msg, adaptor))
        testCompatibility(_.adaptorVerify(badSig, pubKey, msg, adaptor))
    }
  }

  it must "complete adaptor signatures the same" in {
    forAll(CryptoGenerators.privateKey, CryptoGenerators.adaptorSignature) {
      case (adaptorSecret, adaptorSig) =>
        testCompatibility(_.adaptorComplete(adaptorSecret, adaptorSig))
    }
  }

  it must "extract adaptor secrets the same" in {
    forAll(CryptoGenerators.adaptorSignatureWithDecryptedSignatureAndAdaptor) {
      case (adaptorSig, sig, adaptor) =>
        testCompatibility(_.extractAdaptorSecret(sig, adaptorSig, adaptor))
    }
  }
}
