package org.bitcoins.crypto


import org.bitcoins.config.TestNet3
import org.bitcoins.protocol.script.EmptyScriptSignature
import org.bitcoins.util.{BitcoinSUtil, BitcoinJTestUtil, CryptoTestUtil, TestUtil}
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 3/7/16.
 */
class ECFactoryTest extends FlatSpec with MustMatchers {

    "ECFactory" must "create a private key from the dumped base58 in bitcoin-cli" in {
      val privateKeyBase58 = CryptoTestUtil.privateKeyBase58
      val bitcoinjDumpedPrivateKey = new org.bitcoinj.core.DumpedPrivateKey(BitcoinJTestUtil.params,privateKeyBase58)
      val bitcoinjPrivateKey = bitcoinjDumpedPrivateKey.getKey
      val privateKey = ECFactory.fromBase58ToPrivateKey(privateKeyBase58,TestNet3)

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


}
