package org.bitcoins.crypto

import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

/** This is meant to be an abstraction for a [[org.bitcoins.crypto.ECPrivateKey]], sometimes we will not
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
  */
trait AsyncSign {
  def asyncSign(bytes: ByteVector): Future[ECDigitalSignature]

  /** Note that using this function to generate digital signatures with specific
    * properties (by trying a bunch of entropy values) can reduce privacy as it will
    * fingerprint your wallet. Additionally it could lead to a loss of entropy in
    * the resulting nonce should the property you are interested in cause a constraint
    * on the input space.
    *
    * In short, ALL USES OF THIS FUNCTION THAT SIGN THE SAME DATA WITH DIFFERENT ENTROPY
    * HAVE THE POTENTIAL TO CAUSE REDUCTIONS IN SECURITY AND PRIVACY, BEWARE!
    */
  def asyncSignWithEntropy(
      bytes: ByteVector,
      entropy: ByteVector): Future[ECDigitalSignature]

  private def asyncSignLowR(bytes: ByteVector, startAt: Long)(implicit
      ec: ExecutionContext): Future[ECDigitalSignature] = {
    val sigF = if (startAt == 0) { // On first try, use normal signing
      asyncSign(bytes)
    } else { // Subsequently, use additional entropy
      val startBytes = ByteVector.fromLong(startAt).padLeft(32).reverse
      asyncSignWithEntropy(bytes, startBytes)
    }

    sigF.flatMap { sig =>
      if (sig.bytes.length <= 70) {
        Future.successful(sig)
      } else {
        asyncSignLowR(bytes, startAt + 1)
      }
    }
  }

  def asyncSignLowR(bytes: ByteVector)(implicit
      ec: ExecutionContext): Future[ECDigitalSignature] = {
    asyncSignLowR(bytes, startAt = 0)
  }

  def publicKey: ECPublicKey
}

object AsyncSign {

  private case class AsyncSignImpl(
      asyncSignFunction: ByteVector => Future[ECDigitalSignature],
      asyncSignWithEntropyFunction: (
          ByteVector,
          ByteVector) => Future[ECDigitalSignature],
      override val publicKey: ECPublicKey)
      extends AsyncSign {

    override def asyncSign(bytes: ByteVector): Future[ECDigitalSignature] = {
      asyncSignFunction(bytes)
    }

    override def asyncSignWithEntropy(
        bytes: ByteVector,
        entropy: ByteVector): Future[ECDigitalSignature] = {
      asyncSignWithEntropyFunction(bytes, entropy)
    }
  }

  def apply(
      asyncSign: ByteVector => Future[ECDigitalSignature],
      asyncSignWithEntropy: (
          ByteVector,
          ByteVector) => Future[ECDigitalSignature],
      pubKey: ECPublicKey): AsyncSign = {
    AsyncSignImpl(asyncSign, asyncSignWithEntropy, pubKey)
  }

  def constant(sig: ECDigitalSignature, pubKey: ECPublicKey): AsyncSign = {
    AsyncSignImpl(_ => Future.successful(sig),
                  (_, _) => Future.successful(sig),
                  pubKey)
  }

  /** This dummySign function is useful for the case where we do not have the
    * signFunction available on the same jvm as the place where we are creating the
    * sign. I can't think of a good way to serialize the signFunction, so it needs to be
    * optional for now. Maybe we rethink the idea of the signFunction in the future.
    * the public key is still useful here though because it can be used to match against
    * a specific private key on another server
    */
  def dummySign(publicKey: ECPublicKey): AsyncSign = {
    constant(EmptyDigitalSignature, publicKey)
  }
}

trait Sign extends AsyncSign {
  def sign(bytes: ByteVector): ECDigitalSignature

  override def asyncSign(bytes: ByteVector): Future[ECDigitalSignature] = {
    Future.successful(sign(bytes))
  }

  /** Note that using this function to generate digital signatures with specific
    * properties (by trying a bunch of entropy values) can reduce privacy as it will
    * fingerprint your wallet. Additionally it could lead to a loss of entropy in
    * the resulting nonce should the property you are interested in cause a constraint
    * on the input space.
    *
    * In short, ALL USES OF THIS FUNCTION THAT SIGN THE SAME DATA WITH DIFFERENT ENTROPY
    * HAVE THE POTENTIAL TO CAUSE REDUCTIONS IN SECURITY AND PRIVACY, BEWARE!
    */
  def signWithEntropy(
      bytes: ByteVector,
      entropy: ByteVector): ECDigitalSignature

  override def asyncSignWithEntropy(
      bytes: ByteVector,
      entropy: ByteVector): Future[ECDigitalSignature] = {
    Future.successful(signWithEntropy(bytes, entropy))
  }

  @tailrec
  private def signLowR(bytes: ByteVector, startAt: Long): ECDigitalSignature = {
    val sig = if (startAt == 0) { // On first try, use normal signing
      sign(bytes)
    } else { // Subsequently, use additional entropy
      val startBytes = ByteVector.fromLong(startAt).padLeft(32).reverse
      signWithEntropy(bytes, startBytes)
    }

    // If we are using BCrypto then we can't sign with entropy
    CryptoUtil.cryptoContext match {
      case CryptoContext.BCrypto => sig
      case CryptoContext.LibSecp256k1 | CryptoContext.BouncyCastle =>
        if (sig.bytes.length <= 70) {
          sig
        } else {
          signLowR(bytes, startAt + 1)
        }
    }
  }

  def signLowR(bytes: ByteVector): ECDigitalSignature = {
    signLowR(bytes, startAt = 0)
  }

  override def asyncSignLowR(bytes: ByteVector)(implicit
      ec: ExecutionContext): Future[ECDigitalSignature] = {
    Future.successful(signLowR(bytes))
  }
}

object Sign {

  private case class SignImpl(
      signFunction: ByteVector => ECDigitalSignature,
      signWithEntropyFunction: (ByteVector, ByteVector) => ECDigitalSignature,
      override val publicKey: ECPublicKey)
      extends Sign {

    override def sign(bytes: ByteVector): ECDigitalSignature = {
      signFunction(bytes)
    }

    override def signWithEntropy(
        bytes: ByteVector,
        entropy: ByteVector): ECDigitalSignature = {
      signWithEntropyFunction(bytes, entropy)
    }
  }

  def apply(
      sign: ByteVector => ECDigitalSignature,
      signWithEntropy: (ByteVector, ByteVector) => ECDigitalSignature,
      pubKey: ECPublicKey): Sign = {
    SignImpl(sign, signWithEntropy, pubKey)
  }

  def constant(sig: ECDigitalSignature, pubKey: ECPublicKey): Sign = {
    SignImpl(_ => sig, (_, _) => sig, pubKey)
  }

  def dummySign(publicKey: ECPublicKey): Sign = {
    constant(EmptyDigitalSignature, publicKey)
  }
}
