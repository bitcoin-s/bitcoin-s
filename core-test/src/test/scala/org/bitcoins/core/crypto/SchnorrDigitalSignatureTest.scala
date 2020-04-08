package org.bitcoins.core.crypto

import org.bitcoins.testkit.core.gen.{CryptoGenerators, NumberGenerator}
import org.bitcoins.testkit.util.BitcoinSUnitTest

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
}
