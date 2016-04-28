package org.bitcoins.crypto

/**
 * Created by chris on 2/16/16.
 */
sealed trait ECPrivateKey extends BaseECKey {
  import org.bitcoinj.core._

  /**
   * Derives the public for a the private key
   * @return
   */
  def publicKey : ECPublicKey = {
    val bitcoinjECKey : org.bitcoinj.core.ECKey = ECKey.fromPrivate(bytes.toArray)
    ECFactory.publicKey(bitcoinjECKey.getPubKey)
  }

}

sealed case class ECPrivateKeyImpl(hex : String) extends ECPrivateKey
