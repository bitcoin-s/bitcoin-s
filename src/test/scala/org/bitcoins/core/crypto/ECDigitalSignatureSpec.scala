package org.bitcoins.core.crypto

import org.bitcoins.core.gen.CryptoGenerators
import org.scalacheck.{Prop, Properties}

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
      case (hash,key) =>
        val sig = key.sign(hash)
        key.publicKey.verify(hash,sig)
    }
}
