package org.bitcoins.core.crypto

/**
 * Created by chris on 2/16/16.
 */
trait ECKey {
  def privateKey : Option[ECPrivateKey]
  def publicKey : ECPublicKey
}

case class ECKeyImpl(privateKey : Option[ECPrivateKey], publicKey : ECPublicKey) extends ECKey
