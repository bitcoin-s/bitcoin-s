package org.bitcoins.core.crypto

import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.crypto.{AsyncSign, ECDigitalSignature, Sign}
import scodec.bits.ByteVector

import scala.concurrent.Future

/** A signing interface for [[ExtKey]] */
trait AsyncExtSign extends AsyncSign {

  def asyncDeriveAndSign(
      bytes: ByteVector,
      path: BIP32Path): Future[ECDigitalSignature]

  /** First derives the child key that corresponds to [[BIP32Path path]] and then signs */
  def asyncSign(
      bytes: ByteVector,
      path: BIP32Path): Future[ECDigitalSignature] = {
    asyncDeriveAndSign(bytes, path)
  }
}

trait ExtSign extends AsyncExtSign with Sign {

  def deriveAndSign(bytes: ByteVector, path: BIP32Path): ECDigitalSignature

  override def asyncDeriveAndSign(
      bytes: ByteVector,
      path: BIP32Path): Future[ECDigitalSignature] = {
    Future.successful(deriveAndSign(bytes, path))
  }

  /** First derives the child key that corresponds to [[BIP32Path path]] and then signs */
  def sign(bytes: ByteVector, path: BIP32Path): ECDigitalSignature = {
    deriveAndSign(bytes, path)
  }
}
