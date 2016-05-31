package org.bitcoins.core.crypto

import org.bitcoins.core.util.{BitcoinSUtil, CryptoTestUtil}
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

  it must "create a bitcoin-s private key from a bitcoinj private key, then convert to the same public key" in {
    val bitcoinjKey = new org.bitcoinj.core.ECKey()
    val bitcoinsPrivKey = ECFactory.privateKey(bitcoinjKey.getSecretBytes)
    val bitcoinsPublicKey = bitcoinsPrivKey.publicKey
    val bitcoinjPublicKey = bitcoinjKey.getPubKey

    bitcoinsPublicKey.bytes must be (bitcoinjPublicKey)
  }

  it must "create a bitcionj private key from a bitcoins private key and get the same public key" in {
    val bitcoinsPrivKey = ECFactory.privateKey
    val bitcoinjPrivKey = org.bitcoinj.core.ECKey.fromPrivate(bitcoinsPrivKey.bytes.toArray)
    val bitcoinjPublicKey = bitcoinjPrivKey.getPubKey
    val bitcoinsPublicKey = bitcoinsPrivKey.publicKey

    bitcoinsPublicKey.bytes must be (bitcoinjPublicKey)
  }

}
