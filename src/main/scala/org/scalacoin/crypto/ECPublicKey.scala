package org.scalacoin.crypto

/**
 * Created by chris on 2/16/16.
 */
trait ECPublicKey extends BaseECKey

case class ECPublicKeyImpl(bytes : Seq[Byte]) extends ECPublicKey
