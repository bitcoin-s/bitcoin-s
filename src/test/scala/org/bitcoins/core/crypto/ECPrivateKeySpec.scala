package org.bitcoins.core.crypto

import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.gen.CryptoGenerators
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 7/25/16.
  */
class ECPrivateKeySpec extends Properties("ECPrivateKeySpec") {

  property("Serialization symmetry for WIF format") =
    Prop.forAll(CryptoGenerators.privateKey) { privKey =>
      val wif = privKey.toWIF(TestNet3)
      ECPrivateKey.fromWIFToPrivateKey(wif) == privKey
    }
}
