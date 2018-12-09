package org.bitcoins.core.crypto

import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, CryptoTestUtil}
import org.scalatest.{FlatSpec, MustMatchers}
import scodec.bits.ByteVector

/**
  * Created by chris on 3/7/16.
  */
class ECPrivateKeyTest extends FlatSpec with MustMatchers {
  private def logger = BitcoinSLogger.logger

  "ECPrivateKey" must "have the same byte representation as a bitcoinj private key" in {
    val bitcoinjPrivateKey =
      CryptoTestUtil.bitcoinjPrivateKey.getPrivateKeyAsHex
    CryptoTestUtil.privateKey.hex must be(bitcoinjPrivateKey)
  }

  it must "derive the same public from a private key as bitcoinj" in {
    val bitcoinjPublicKeyBytes =
      ByteVector(CryptoTestUtil.bitcoinjPrivateKey.getPubKey)
    CryptoTestUtil.privateKey.publicKey.hex must be(
      BitcoinSUtil.encodeHex(bitcoinjPublicKeyBytes))
  }

  it must "create a bitcoin-s private key from a bitcoinj private key, then convert to the same public key" in {
    val bitcoinjKey = new org.bitcoinj.core.ECKey()
    val bitcoinsPrivKey = ECPrivateKey(ByteVector(bitcoinjKey.getSecretBytes))
    val bitcoinsPublicKey = bitcoinsPrivKey.publicKey
    val bitcoinjPublicKey = bitcoinjKey.getPubKey

    bitcoinsPublicKey.bytes.toArray must be(bitcoinjPublicKey)
  }

  it must "create a bitcionj private key from a bitcoins private key and get the same public key" in {
    val bitcoinsPrivKey = ECPrivateKey.freshPrivateKey
    val bitcoinjPrivKey =
      org.bitcoinj.core.ECKey.fromPrivate(bitcoinsPrivKey.bytes.toArray)
    val bitcoinjPublicKey = bitcoinjPrivKey.getPubKey
    val bitcoinsPublicKey = bitcoinsPrivKey.publicKey

    bitcoinsPublicKey.bytes.toArray must be(bitcoinjPublicKey)
  }

  it must "create a private key from the dumped base58 in bitcoin-cli" in {
    val bitcoinjDumpedPrivateKey = CryptoTestUtil.bitcoinjDumpedPrivateKey
    val bitcoinjPrivateKey = bitcoinjDumpedPrivateKey.getKey
    val privateKey =
      ECPrivateKey.fromWIFToPrivateKey(CryptoTestUtil.privateKeyBase58)
    privateKey.hex must be(bitcoinjPrivateKey.getPrivateKeyAsHex)
  }

  it must "create a private key from a sequence of bytes that has the same byte representation of bitcoinj ECKeys" in {
    val bitcoinJKey = CryptoTestUtil.bitcoinjPrivateKey
    val privateKey: ECPrivateKey =
      ECPrivateKey(ByteVector(bitcoinJKey.getPrivKeyBytes))
    privateKey.hex must be(bitcoinJKey.getPrivateKeyAsHex)
  }

  it must "create a private key from its hex representation" in {
    val privateKeyHex =
      "180cb41c7c600be951b5d3d0a7334acc7506173875834f7a6c4c786a28fcbb19"
    val key: ECPrivateKey = ECPrivateKey(privateKeyHex)
    key.hex must be(privateKeyHex)
  }

  it must "determine if a private key corresponds to a compressed public key or not" in {
    val compressedKey = "L1RrrnXkcKut5DEMwtDthjwRcTTwED36thyL1DebVrKuwvohjMNi"
    val uncompressedKey = "93DVKyFYwSN6wEo3E2fCrFPUp17FtrtNi2Lf7n4G3garFb16CRj"
    ECPrivateKey.isCompressed(compressedKey) must be(true)
    ECPrivateKey.isCompressed(uncompressedKey) must be(false)
  }

  it must "create a fresh private key" in {
    ECPrivateKey() must not equal (ECPrivateKey())
  }

  it must "serialize a private key to WIF and then be able to deserialize it" in {
    val hex = "2cecbfb72f8d5146d7fe7e5a3f80402c6dd688652c332dff2e44618d2d3372"
    val privKey = ECPrivateKey(hex)
    val wif = privKey.toWIF(TestNet3)
    val privKeyFromWIF = ECPrivateKey.fromWIFToPrivateKey(wif)
    privKeyFromWIF must be(privKey)
  }

  it must "serialize a private key to WIF when the private key is prefixed with 0 bytes" in {
    val hex =
      "00fc391adf4d6063a16a2e38b14d2be10133c4dacd4348b49d23ee0ce5ff4f1701"
    val privKey = ECPrivateKey(hex)
    val wif = privKey.toWIF(TestNet3)
    val privKeyFromWIF = ECPrivateKey.fromWIFToPrivateKey(wif)
    privKeyFromWIF must be(privKey)
  }

  it must "correctly decode a private key from WIF" in {
    val privateKey = ECPrivateKey.fromWIFToPrivateKey(
      "cTPg4Zc5Jis2EZXy3NXShgbn487GWBTapbU63BerLDZM3w2hQSjC")
    //derived hex on bitcore's playground
    privateKey.hex must be(
      "ad59fb6aadf617fb0f93469741fcd9a9f48700f1d1f465ddc0f26fa7f7bfa1ac")
  }

  it must "decode a WIF private key corresponding to uncompressed public key" in {
    val wif = "5Kg1gnAjaLfKiwhhPpGS3QfRg2m6awQvaj98JCZBZQ5SuS2F15C"
    val privKey = ECPrivateKey.fromWIFToPrivateKey(wif)
    privKey.publicKey.hex must be(
      "045b81f0017e2091e2edcd5eecf10d5bdd120a5514cb3ee65b8447ec18bfc4575c6d5bf415e54e03b1067934a0f0ba76b01c6b9ab227142ee1d543764b69d901e0")
  }

}
