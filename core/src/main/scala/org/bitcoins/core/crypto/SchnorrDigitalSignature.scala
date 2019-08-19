package org.bitcoins.core.crypto

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, Factory}
import scodec.bits.ByteVector

sealed abstract class SchnorrDigitalSignature extends NetworkElement {
  require(rx.length == 32, s"R must be 32 bytes, got $rx")
  require(s.length == 32, s"s must be 32 bytes, got $s")
  require(bytes.length == 64, s"This must be 64 bytes, got $bytes")

  /** Represents the x coordinate of the R value used in the signature */
  def rx: ByteVector

  /** The s = k + e*x value of the signature */
  def s: ByteVector

  override def bytes: ByteVector = rx ++ s

  override def toString: String = s"SchnorrDigitalSignature($rx, $s)"
}

final case object DummySchnorrDigitalSignature extends SchnorrDigitalSignature {
  override val rx: ByteVector = ByteVector.fill(32)(0.toByte)
  override val s: ByteVector = rx
}

object SchnorrDigitalSignature extends Factory[SchnorrDigitalSignature] {
  private case class SchnorrDigitalSignatureImpl(rx: ByteVector, s: ByteVector)
      extends SchnorrDigitalSignature

  override def fromBytes(bytes: ByteVector): SchnorrDigitalSignature = {
    if (bytes.length != 64) {
      throw new IllegalArgumentException(
        s"SchnorrDigitalSignature must be 64 bytes, got $bytes")
    } else {
      val (rx, s) = bytes.splitAt(32)
      SchnorrDigitalSignatureImpl(rx, s)
    }
  }

  /** Constructs a SchnorrDigitalSignature from the x-coordinate of the R
    * value and the s value.
    */
  def fromRxS(rx: ByteVector, s: ByteVector): SchnorrDigitalSignature = {
    if (rx.length != 32) {
      throw new IllegalArgumentException(s"rx must be 32 bytes, got $rx")
    } else if (s.length != 32) {
      throw new IllegalArgumentException(s"s must be 32 bytes, got $s")
    } else {
      SchnorrDigitalSignatureImpl(rx, s)
    }
  }

  def apply(rx: ByteVector, s: ByteVector): SchnorrDigitalSignature = {
    fromRxS(rx, s)
  }

  /** Consturcts a SchnorrDigitalSignature form the R and s values. */
  def fromRS(r: ECPublicKey, s: ByteVector): SchnorrDigitalSignature = {
    if (s.length != 32) {
      throw new IllegalArgumentException(s"s must be 32 bytes, got $s")
    } else {
      fromRxS(r.bytes.tail, s)
    }
  }

  def apply(r: ECPublicKey, s: ByteVector): SchnorrDigitalSignature = {
    fromRS(r, s)
  }
}
