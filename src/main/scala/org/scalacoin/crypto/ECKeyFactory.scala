package org.scalacoin.crypto

import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 2/16/16.
 */
object ECKeyFactory {

  def privateKey(hex : String) : ECPrivateKey = privateKey(ScalacoinUtil.decodeHex(hex))

  def privateKey(bytes : Seq[Byte]) : ECPrivateKey = ECPrivateKeyImpl(ScalacoinUtil.encodeHex(bytes),bytes)

  def privateKey(bytes : List[Byte]) : ECPrivateKey = privateKey(bytes.toSeq)

  def publicKey(hex : String) : ECPublicKey = publicKey(ScalacoinUtil.decodeHex(hex))

  def publicKey(bytes : Seq[Byte]) : ECPublicKey = ECPublicKeyImpl(ScalacoinUtil.encodeHex(bytes),bytes)

  def publicKey(bytes : List[Byte]) : ECPublicKey = publicKey(bytes.toSeq)

}
