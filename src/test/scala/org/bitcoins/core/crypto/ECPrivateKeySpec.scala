package org.bitcoins.core.crypto

import org.bitcoins.core.config.{MainNet, TestNet3}
import org.bitcoins.core.gen.{ChainParamsGenerator, CryptoGenerators}
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 7/25/16.
  */
class ECPrivateKeySpec extends Properties("ECPrivateKeySpec") {

  property("Serialization symmetry for WIF format") =
    Prop.forAll(CryptoGenerators.privateKey, ChainParamsGenerator.networkParams) { (privKey,network) =>
      val wif = privKey.toWIF(network)
      val isCorrectNetwork = if (network == MainNet) {
        ECPrivateKey.parseNetworkFromWIF(wif).get == network
      } else {
        // we need this hack because RegTest & TestNet3 have the same base58 prefixes
        ECPrivateKey.parseNetworkFromWIF(wif).get == TestNet3
      }
      ECPrivateKey.fromWIFToPrivateKey(wif) == privKey && isCorrectNetwork

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
