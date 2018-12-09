package org.bitcoins.core.crypto

import scodec.bits.ByteVector

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

/**
  * This is meant to be an abstraction for a [[org.bitcoins.core.crypto.ECPrivateKey]], sometimes we will not
  * have direct access to a private key in memory -- for instance if that key is on a hardware device -- so we need to create an
  * abstraction of the signing process. Fundamentally a private key takes in a scodec.bits.ByteVector and returns a [[ECDigitalSignature]]
  * That is what this abstraction is meant to represent. If you have a [[ECPrivateKey]] in your application, you can get it's
  * [[Sign]] type by doing this:
  *
  * val key = ECPrivateKey()
  * val sign: scodec.bits.ByteVector => Future[ECDigitalSignature] = key.signFunction
  *
  * If you have a hardware wallet, you will need to implement the protocol to send a message to the hardware device. The
  * type signature of the function you implement must be scodec.bits.ByteVector => Future[ECDigitalSignature]
  *
  */
trait Sign {
  def signFunction: ByteVector => Future[ECDigitalSignature]

  def signFuture(bytes: ByteVector): Future[ECDigitalSignature] =
    signFunction(bytes)

  def sign(bytes: ByteVector): ECDigitalSignature = {
    Await.result(signFuture(bytes), 30.seconds)
  }

  def publicKey: ECPublicKey
}

object Sign {
  private case class SignImpl(
      signFunction: ByteVector => Future[ECDigitalSignature],
      publicKey: ECPublicKey)
      extends Sign

  def apply(
      signFunction: ByteVector => Future[ECDigitalSignature],
      pubKey: ECPublicKey): Sign = {
    SignImpl(signFunction, pubKey)
  }

  /**
    * This dummySign function is useful for the case where we do not have the
    * signFunction available on the same jvm as the place where we are creating the
    * sign. I can't think of a good way to serialize the signFunction, so it needs to be
    * optional for now. Maybe we rethink the idea of the signFunction in the future.
    * the public key is still useful here though because it can be used to match against
    * a specific private key on another server
    */
  def dummySign(publicKey: ECPublicKey): Sign = {
    SignImpl({ _: ByteVector =>
      Future.successful(EmptyDigitalSignature)
    }, publicKey)
  }
}
