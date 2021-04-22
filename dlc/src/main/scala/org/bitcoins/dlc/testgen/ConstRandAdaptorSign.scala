package org.bitcoins.dlc.testgen

import org.bitcoins.crypto._
import scodec.bits.ByteVector

/** Wraps ECPrivateKey signing functionality but where adaptorSign uses a
  * constant ByteVector for auxRand resulting in deterministic adaptor signing.
  */
case class ConstRandAdaptorSign(privKey: ECPrivateKey) extends AdaptorSign {

  override def adaptorSign(
      adaptorPoint: ECPublicKey,
      msg: ByteVector): ECAdaptorSignature = {
    adaptorSign(adaptorPoint, msg, ConstRandAdaptorSign.constRand)
  }

  override def sign(bytes: ByteVector): ECDigitalSignature = {
    privKey.sign(bytes)
  }

  override def publicKey: ECPublicKey = {
    privKey.publicKey
  }

  override def signWithEntropy(
      bytes: ByteVector,
      entropy: ByteVector): ECDigitalSignature = {
    privKey.signWithEntropy(bytes, entropy)
  }

  override def adaptorSign(
      adaptorPoint: ECPublicKey,
      msg: ByteVector,
      auxRand: ByteVector): ECAdaptorSignature = {
    privKey.adaptorSign(adaptorPoint, msg, auxRand)
  }
}

object ConstRandAdaptorSign {

  val constRand: ByteVector =
    CryptoUtil.serializeForHash("DLC_TEST").padLeft(32)
}
