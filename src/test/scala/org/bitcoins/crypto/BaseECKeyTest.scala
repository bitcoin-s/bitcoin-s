package org.bitcoins.crypto

import org.bitcoinj.core.ECKey.ECDSASignature
import org.bitcoinj.core.Sha256Hash
import org.bitcoins.util.BitcoinSUtil
import org.scalatest.{FlatSpec, MustMatchers}
/**
 * Created by chris on 2/29/16.
 */
class BaseECKeyTest extends FlatSpec with MustMatchers  {

  "BaseECKey" must "sign a arbitrary piece of data" in {
    //follows this bitcoinj test case
    //https://github.com/bitcoinj/bitcoinj/blob/master/core/src/test/java/org/bitcoinj/core/ECKeyTest.java#L110
    val privateKeyHex = "180cb41c7c600be951b5d3d0a7334acc7506173875834f7a6c4c786a28fcbb19"
    val key: BaseECKey = ECFactory.fromHex(privateKeyHex)
    val signature: ECDigitalSignature = key.sign(Sha256Hash.ZERO_HASH.getBytes.toSeq)

    val bitcoinjKey = org.bitcoinj.core.ECKey.fromPrivate(BitcoinSUtil.decodeHex(privateKeyHex).toArray)
    val bitcoinjSignature: ECDSASignature = bitcoinjKey.sign(Sha256Hash.ZERO_HASH)
    signature.hex must be (BitcoinSUtil.encodeHex(bitcoinjSignature.encodeToDER()))

  }

  it must "sign a hex string" in {
    val key = ECFactory.privateKey
    val signature = key.sign("180cb41c7c600be951b5d3d0a7334acc7506173875834f7a6c4c786a28fcbb19")
    key.publicKey.verify("180cb41c7c600be951b5d3d0a7334acc7506173875834f7a6c4c786a28fcbb19", signature) must be (true)
  }

  it must "sign the hex string with an explicitly given private key" in {
    val key1 = ECFactory.privateKey
    val key2 = ECFactory.privateKey
    val signature = key1.sign("180cb41c7c600be951b5d3d0a7334acc7506173875834f7a6c4c786a28fcbb19",key2)
    key2.publicKey.verify("180cb41c7c600be951b5d3d0a7334acc7506173875834f7a6c4c786a28fcbb19", signature) must be (true)
  }
}
