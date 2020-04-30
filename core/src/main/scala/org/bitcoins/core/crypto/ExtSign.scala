package org.bitcoins.core.crypto

import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.crypto.{ECDigitalSignature, Sign}
import scodec.bits.ByteVector

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

/** A signing interface for [[ExtKey]] */
trait ExtSign extends Sign {

  def deriveAndSignFuture: (ByteVector, BIP32Path) => Future[ECDigitalSignature]

  /** First derives the child key that corresponds to [[BIP32Path path]] and then signs */
  def sign(bytes: ByteVector, path: BIP32Path): ECDigitalSignature = {
    Await.result(deriveAndSignFuture(bytes, path), 30.seconds)
  }
}
