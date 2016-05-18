package org.bitcoins.core.crypto


import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.protocol.script.EmptyScriptSignature
import org.bitcoins.core.util.{BitcoinSUtil, BitcoinJTestUtil, CryptoTestUtil, TestUtil}
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 3/7/16.
 */
class ECFactoryTest extends FlatSpec with MustMatchers {

  "ECFactory" must "create a private key from the dumped base58 in bitcoin-cli" in {
    val privateKeyBase58 = CryptoTestUtil.privateKeyBase58
    val bitcoinjDumpedPrivateKey = new org.bitcoinj.core.DumpedPrivateKey(BitcoinJTestUtil.params,privateKeyBase58)
    val bitcoinjPrivateKey = bitcoinjDumpedPrivateKey.getKey
    val privateKey = ECFactory.fromBase58ToPrivateKey(privateKeyBase58)

    privateKey.hex must be (bitcoinjPrivateKey.getPrivateKeyAsHex)

  }

  it must "create a private key from a sequence of bytes that has the same byte representation of bitcoinj ECKeys" in {
    val bytes = CryptoTestUtil.bitcoinjPrivateKey.getPrivKeyBytes.toList
    val bitcoinJKey = org.bitcoinj.core.ECKey.fromPrivate(bytes.toArray)
    val privateKey : ECPrivateKey = ECFactory.privateKey(bytes)
    privateKey.hex must be (bitcoinJKey.getPrivateKeyAsHex)
  }

  it must "create an empty digital signature when given 0 in hex or byte format" in {
    val hex = ECFactory.digitalSignature("00")
    val byte = ECFactory.digitalSignature(Seq(0.toByte))
    val emptySignature = ECFactory.digitalSignature("")
    byte must be (emptySignature)
    hex must be (emptySignature)
  }

  it must "create a private key from bytes" in {
    val privKeyBytes = Seq[Byte](0.toByte)
    ECFactory.privateKey(privKeyBytes).bytes must be (privKeyBytes)
  }

  it must "create a private key from its hex representation" in {
    val privateKeyHex = "180cb41c7c600be951b5d3d0a7334acc7506173875834f7a6c4c786a28fcbb19"
    val key: ECPrivateKey = ECFactory.privateKey(privateKeyHex)
    key.hex must be (privateKeyHex)
  }

  it must "create a digital signature from it's r,s components" in {
    //from the tx 44e504f5b7649d215be05ad9f09026dee95201244a3b218013c504a6a49a26ff
    val rawDigitalSignature = "3044022040f91c48f4011bf2e2edb6621bfa8fb802241de939cb86f1872c99c580ef0fe402204fc27388bc525e1b655b5f5b35f9d601d28602432dd5672f29e0a47f5b8bbb26"
    val digitalSignature = ECFactory.digitalSignature(rawDigitalSignature)
    val (r,s) = (digitalSignature.r, digitalSignature.s)
    val digitalSignatureFromRS = ECFactory.digitalSignature(r,s)
    digitalSignatureFromRS must be (digitalSignature)
  }




}
