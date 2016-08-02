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

  property("Serialization symmetry") =
    Prop.forAll(CryptoGenerators.privateKey) { privKey =>
      ECPrivateKey(privKey.hex) == privKey
    }

  property("unique key generation") =
    Prop.forAll(CryptoGenerators.privateKey, CryptoGenerators.privateKey) { (privKey1, privKey2) =>
      privKey1 != privKey2
    }
}
