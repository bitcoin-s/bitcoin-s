package org.scalacoin.crypto

/**
 * Created by chris on 2/16/16.
 */
trait ECPrivateKey extends BaseECKey {
  def publicKey : ECPublicKey = ???
}

case class ECPrivateKeyImpl(bytes : Seq[Byte]) extends ECPrivateKey
