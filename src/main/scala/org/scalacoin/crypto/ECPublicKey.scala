package org.scalacoin.crypto

/**
 * Created by chris on 2/16/16.
 */
trait ECPublicKey extends BaseECKey

case class ECPublicKeyImpl(hex : String, bytes : Seq[Byte]) extends ECPublicKey
