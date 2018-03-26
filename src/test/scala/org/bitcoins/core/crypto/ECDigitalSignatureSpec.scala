package org.bitcoins.core.crypto

import org.bitcoins.core.gen.CryptoGenerators
import org.scalacheck.{ Prop, Properties }

/**
 * Created by chris on 8/16/16.
 */
class ECDigitalSignatureSpec extends Properties("ECDigitalSignatureSpec") {

  property("must be der encoded") =
    Prop.forAll(CryptoGenerators.digitalSignature) { signature =>
      signature.isDEREncoded
    }

  property("must have a low s") =
    Prop.forAll(CryptoGenerators.digitalSignature) { signature =>
      DERSignatureUtil.isLowS(signature)
    }

  property("must create and verify a digital signature") =
    Prop.forAll(CryptoGenerators.doubleSha256Digest, CryptoGenerators.privateKey) {
      case (hash, key) =>
        val sig = key.sign(hash)
        key.publicKey.verify(hash, sig)
    }

  property("must not reuse r values") = {
    Prop.forAll(CryptoGenerators.privateKey, CryptoGenerators.doubleSha256Digest, CryptoGenerators.doubleSha256Digest) {
      case (key, hash1, hash2) =>
        val sig1 = key.sign(hash1)
        val sig2 = key.sign(hash2)
        sig1.r != sig2.r
    }
  }
}
