package org.bitcoins.core.crypto

import org.bitcoins.core.protocol.NetworkElement
import scodec.bits.ByteVector

case class SchnorrDigitalSignature(rx: ByteVector, s: ByteVector)
    extends NetworkElement {
  require(rx.length == 32, s"R must be 32 bytes, got $rx")
  require(s.length == 32, s"s must be 32 bytes, got $s")
  require(bytes.length == 64, s"This must be 64 bytes, got $bytes")

  override def bytes: ByteVector = rx ++ s
}

final object DummySchnorrDigitalSignature
    extends SchnorrDigitalSignature(ByteVector.low(32), ByteVector.low(32))

object SchnorrDigitalSignature {

  def fromBytes(bytes: ByteVector): SchnorrDigitalSignature = {
    require(bytes.length == 64,
            s"SchnorrDigitalSignature must be 64 bytes, got $bytes")

    val (rx, s) = bytes.splitAt(32)
    SchnorrDigitalSignature(rx, s)
  }

  def apply(bytes: ByteVector): SchnorrDigitalSignature = {
    fromBytes(bytes)
  }

  /** Constructs a SchnorrDigitalSignature from the x-coordinate of the R
    * value and the s value.
    */
  def fromRxS(rx: ByteVector, s: ByteVector): SchnorrDigitalSignature = {
    require(rx.length == 32, s"rx must be 32 bytes, got $rx")
    require(s.length == 32, s"s must be 32 bytes, got $s")

    SchnorrDigitalSignature(rx, s)
  }

  /** Consturcts a SchnorrDigitalSignature form the R and s values. */
  def fromRS(r: ECPublicKey, s: ByteVector): SchnorrDigitalSignature = {
    require(s.length == 32, s"s must be 32 bytes, got $s")

    fromRxS(r.bytes.tail, s)
  }

  def apply(r: ECPublicKey, s: ByteVector): SchnorrDigitalSignature = {
    fromRS(r, s)
  }
}
