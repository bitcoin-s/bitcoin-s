package org.bitcoins.core.crypto

import org.bitcoinj.core.Sha256Hash
import org.bitcoins.core.gen.CryptoGenerators
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}
import scodec.bits.ByteVector

/**
  * Created by chris on 2/29/16.
  */
class ECPublicKeyTest extends FlatSpec with MustMatchers {

  "ECPublicKey" must "verify that a arbitrary piece of data was signed by the private key corresponding to a public key" in {

    val privateKeyHex =
      "180cb41c7c600be951b5d3d0a7334acc7506173875834f7a6c4c786a28fcbb19"
    val key: ECPrivateKey = ECPrivateKey(privateKeyHex)

    val hash = DoubleSha256Digest(ByteVector(Sha256Hash.ZERO_HASH.getBytes))
    val signature: ECDigitalSignature = key.sign(hash)

    val isValid: Boolean =
      key.publicKey.verify(ByteVector(Sha256Hash.ZERO_HASH.getBytes), signature)
    isValid must be(true)
  }

  it must "fail to verify a piece of data if the wrong public key is given" in {
    val privateKeyHex =
      "180cb41c7c600be951b5d3d0a7334acc7506173875834f7a6c4c786a28fcbb19"
    val key: ECPrivateKey = ECPrivateKey(privateKeyHex)
    val hash = DoubleSha256Digest(ByteVector(Sha256Hash.ZERO_HASH.getBytes))
    val signature: ECDigitalSignature = key.sign(hash)

    val wrongPublicKey = ECPublicKey.freshPublicKey
    val isValid: Boolean = wrongPublicKey.verify(hash, signature)
    isValid must be(false)
  }

  it must "verify a piece of data signed with a bitcoinj private key" in {
    val bitcoinjPrivKey = new org.bitcoinj.core.ECKey
    val bitcoinjSignature = bitcoinjPrivKey.sign(Sha256Hash.ZERO_HASH)
    val bitcoinsSignature =
      ECDigitalSignature(ByteVector(bitcoinjSignature.encodeToDER()))
    val bitcoinsPublicKey = ECPublicKey(ByteVector(bitcoinjPrivKey.getPubKey))
    bitcoinsPublicKey.verify(ByteVector(Sha256Hash.ZERO_HASH.getBytes),
                             bitcoinsSignature) must be(true)

  }

  it must "verify a piece of data was signed with a bitcoins private key inside of bitcoinj" in {
    val bitcoinsPrivKey = ECPrivateKey.freshPrivateKey
    val hash = DoubleSha256Digest(ByteVector(Sha256Hash.ZERO_HASH.getBytes))
    val bitcoinsSignature = bitcoinsPrivKey.sign(hash)
    val bitcoinjPublicKey = org.bitcoinj.core.ECKey
      .fromPublicOnly(bitcoinsPrivKey.publicKey.bytes.toArray)
    bitcoinjPublicKey.verify(Sha256Hash.ZERO_HASH.getBytes,
                             bitcoinsSignature.bytes.toArray) must be(true)
  }

  it must "have serialization symmetry from ECPublicKey -> ECPoint -> ECPublicKey" in {
    PropertyChecks.forAll(CryptoGenerators.publicKey) { pubKey =>
      val p = pubKey.toPoint
      val pub2 = ECPublicKey.fromPoint(p, pubKey.isCompressed)
      assert(pubKey == pub2)
    }
  }
}
