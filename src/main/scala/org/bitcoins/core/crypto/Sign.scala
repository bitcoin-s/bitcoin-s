package org.bitcoins.core.crypto

import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration.DurationInt

/**
 * This is meant to be an abstraction for a [[org.bitcoins.core.crypto.ECPrivateKey]],f sometimes we will not
 * have direct access to a private key in memory -- for instance if that key is on a hardware device -- so we need to create an
 * abstraction of the signing process. Fundamentally a private key takes in a Seq[Byte] and returns a [[ECDigitalSignature]]
 * That is what this abstraction is meant to represent. If you have a [[ECPrivateKey]] in your application, you can get it's
 * [[Sign]] type by doing this:
 *
 * val key = ECPrivateKey()
 * val sign: Seq[Byte] => Future[ECDigitalSignature] = key.signFunction
 *
 * If you have a hardware wallet, you will need to implement the protocol to send a message to the hardware device. The
 * type signature of the function you implement must be Seq[Byte] => Future[ECDigitalSignature]
 *
 */
trait Sign {
  def signFunction: Seq[Byte] => Future[ECDigitalSignature]

  def signFuture(bytes: Seq[Byte]): Future[ECDigitalSignature] = signFunction(bytes)

  def sign(bytes: Seq[Byte]): ECDigitalSignature = {
    Await.result(signFuture(bytes), 30.seconds)
  }

  def publicKey: ECPublicKey
}

object Sign {
  private case class SignImpl(signFunction: Seq[Byte] => Future[ECDigitalSignature], publicKey: ECPublicKey) extends Sign

  def apply(signFunction: Seq[Byte] => Future[ECDigitalSignature], pubKey: ECPublicKey): Sign = {
    SignImpl(signFunction, pubKey)
  }
}
