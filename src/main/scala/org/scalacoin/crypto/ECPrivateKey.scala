package org.scalacoin.crypto

/**
 * Created by chris on 2/16/16.
 */
trait ECPrivateKey extends BaseECKey {
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

case class ECPrivateKeyImpl(bytes : Seq[Byte]) extends ECPrivateKey
