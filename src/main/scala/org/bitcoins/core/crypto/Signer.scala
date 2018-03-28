package org.bitcoins.core.crypto

import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration.DurationInt

trait Sign {
  def signFunction: Seq[Byte] => Future[ECDigitalSignature]

  def signFuture(bytes: Seq[Byte])(implicit ec: ExecutionContext): Future[ECDigitalSignature] = signFunction(bytes)

  def sign(bytes: Seq[Byte]): ECDigitalSignature = {
    //TODO: review this later, is this ok?
    import scala.concurrent.ExecutionContext.Implicits.global
    Await.result(signFuture(bytes)(global), 30.seconds)
  }

  def pubKeyOpt: Option[ECPublicKey]

  implicit val ec: ExecutionContext
}

/**
 * This is meant to be an abstraction for a [[org.bitcoins.core.crypto.ECPrivateKey]], sometimes we will not
 * have direct access to a private key in memory -- for instance if that key is on a hardware device -- so we need to create an
 * abstraction of the signing process. Fundamentally a private key takes in a Seq[Byte] and returns a [[ECDigitalSignature]]
 * That is what this abstraction is meant to represent. If you have a [[ECPrivateKey]] in your application, you can get it's
 * [[Sign]] type by doing this:
 *
 * val key = ECPrivateKey()
 * val sign: (Seq[Byte] => ECDigitalSignature, Option[ECPublicKey]) = (key.sign(_: Seq[Byte]), key.publicKey)
 *
 * If you have a hardware wallet, you will need to implement the protocol to send a message to the hardware device. The
 * type signature of the function you implement must be Seq[Byte] => ECDigitalSignature
 *
 */
object Sign {
  private case class SignImpl(signFunction: Seq[Byte] => Future[ECDigitalSignature], pubKeyOpt: Option[ECPublicKey], implicit val ec: ExecutionContext) extends Sign

  def apply(signFunction: Seq[Byte] => Future[ECDigitalSignature], pubKeyOpt: Option[ECPublicKey]): Sign = {
    val ec = scala.concurrent.ExecutionContext.Implicits.global
    Sign(signFunction, pubKeyOpt, ec)
  }

  def apply(signFunction: Seq[Byte] => Future[ECDigitalSignature]): Sign = {
    Sign(signFunction, None)
  }

  def apply(signFunction: Seq[Byte] => Future[ECDigitalSignature], pubKeyOpt: Option[ECPublicKey], ec: ExecutionContext): Sign = {
    SignImpl(signFunction, pubKeyOpt, ec)
  }
}
