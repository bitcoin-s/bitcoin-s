package org.bitcoins.core.crypto

import org.bitcoins.core.gen.CryptoGenerators
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 8/16/16.
  */
class ECDigitalSignatureSpec extends Properties("ECDigitalSignatureSpec") {

  property("must be der encoded") =
    Prop.forAll(CryptoGenerators.digitalSignatures) { signature =>
      signature.isDEREncoded
    }

  property("must have a low s") =
    Prop.forAll(CryptoGenerators.digitalSignatures) { signature =>
      DERSignatureUtil.isLowS(signature)

    }
}
