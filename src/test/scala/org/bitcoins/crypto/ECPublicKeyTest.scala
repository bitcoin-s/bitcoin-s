package org.bitcoins.crypto

import org.bitcoinj.core.Sha256Hash
import org.bitcoins.util.BitcoinSUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/29/16.
 */
class ECPublicKeyTest extends FlatSpec with MustMatchers {

  "ECPublicKey" must "verify that a arbitrary piece of data was signed by the private key corresponding to a public key" in {

    val privateKeyHex = "180cb41c7c600be951b5d3d0a7334acc7506173875834f7a6c4c786a28fcbb19"
    val key: ECPrivateKey = ECFactory.privateKey(privateKeyHex)
    val signature: ECDigitalSignature = key.sign(Sha256Hash.ZERO_HASH.getBytes.toSeq)

    val isValid : Boolean = key.publicKey.verify(Sha256Hash.ZERO_HASH.getBytes.toSeq,signature)
    isValid must be (true)
  }



  it must "fail to verify a piece of data if the wrong public key is given" in {
    val privateKeyHex = "180cb41c7c600be951b5d3d0a7334acc7506173875834f7a6c4c786a28fcbb19"
    val key: ECPrivateKey = ECFactory.privateKey(privateKeyHex)
    val signature: ECDigitalSignature = key.sign(Sha256Hash.ZERO_HASH.getBytes.toSeq)

    val wrongPublicKey = ECFactory.publicKey
    val isValid : Boolean = wrongPublicKey.verify(Sha256Hash.ZERO_HASH.getBytes.toSeq,signature)
    isValid must be (false)
  }

}
