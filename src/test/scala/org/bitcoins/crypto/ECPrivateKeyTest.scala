package org.bitcoins.crypto

import org.bitcoins.util.{BitcoinSUtil, CryptoTestUtil}
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 3/7/16.
 */
class ECPrivateKeyTest extends FlatSpec with MustMatchers {

  "ECPrivateKey" must "have the same byte representation as a bitcoinj private key" in {
    val bitcoinjPrivateKey = CryptoTestUtil.bitcoinjPrivateKey.getPrivateKeyAsHex
    CryptoTestUtil.privateKey.hex must be (bitcoinjPrivateKey)

  }



  it must "derive the same public from a private key as bitcoinj" in {
    val bitcoinjPublicKeyBytes = CryptoTestUtil.bitcoinjPrivateKey.getPubKey
    CryptoTestUtil.privateKey.publicKey.hex must be (BitcoinSUtil.encodeHex(bitcoinjPublicKeyBytes))

  }

}
