package org.bitcoins.crypto

import org.bitcoinj.core.Sha256Hash
import org.bitcoins.util.{BitcoinSUtil}

/**
 * Created by chris on 2/16/16.
 */
trait BaseECKey {
  import org.bitcoinj.core.ECKey
  def hex : String

  def bytes : Seq[Byte] = BitcoinSUtil.decodeHex(hex)

  /**
   * Signs a given sequence of bytes with the signingKey
 *
   * @param bytes
   * @param signingKey
   * @return
   */
  def sign(bytes : Seq[Byte],signingKey : BaseECKey) : ECDigitalSignature = {
    val bitcoinjKey = ECKey.fromPrivate(signingKey.bytes.toArray)
    val sha256Hash = Sha256Hash.wrap(bytes.toArray)
    val sigBytes : Array[Byte] = bitcoinjKey.sign(sha256Hash).encodeToDER()
    ECFactory.digitalSignature(sigBytes.toSeq)
  }

  def sign(hex : String, signingKey : BaseECKey) : ECDigitalSignature = sign(BitcoinSUtil.decodeHex(hex),signingKey)

  def sign(hex : String) : ECDigitalSignature = sign(hex,this)

  def sign(bytes : Seq[Byte]) : ECDigitalSignature = sign(bytes,this)


}
