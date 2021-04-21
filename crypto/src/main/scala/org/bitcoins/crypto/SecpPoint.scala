package org.bitcoins.crypto

import scodec.bits.ByteVector

import java.math.BigInteger

/** Represents a point on secp256k1 elliptic curve.
  */
sealed trait SecpPoint extends NetworkElement {

  def add(point: SecpPoint): SecpPoint = {
    CryptoUtil.add(this, point)
  }
}

/** The infinity point.
  */
case object SecpPointInfinity extends SecpPoint {
  override def bytes: ByteVector = ByteVector(0x00)
}

/** A point on an elliptic curve.
  * @param x
  * @param y
  */
case class SecpPointFinite(x: FieldElement, y: FieldElement) extends SecpPoint {

  override def bytes: ByteVector = {
    ByteVector(0x04) ++ x.bytes ++ y.bytes
  }

  def toPublicKey: ECPublicKey = {
    ECPublicKey(bytes)
  }
}

object SecpPoint {

  def fromPublicKey(key: ECPublicKey): SecpPointFinite = {
    val (x, y) = key.decompressedBytes.tail.splitAt(32)
    SecpPointFinite(FieldElement.fromBytes(x), FieldElement.fromBytes(y))
  }

  def apply(x: ByteVector, y: ByteVector): SecpPointFinite =
    SecpPointFinite(FieldElement.fromBytes(x), FieldElement.fromBytes(y))

  def apply(x: Array[Byte], y: Array[Byte]): SecpPointFinite =
    SecpPointFinite(FieldElement.fromByteArray(x),
                    FieldElement.fromByteArray(y))

  def apply(x: BigInteger, y: BigInteger): SecpPointFinite =
    SecpPointFinite(FieldElement(x), FieldElement(y))

  def apply(x: BigInt, y: BigInt): SecpPointFinite =
    SecpPointFinite(FieldElement(x), FieldElement(y))

  def apply(x: String, y: String): SecpPointFinite =
    SecpPointFinite(FieldElement.fromHex(x), FieldElement.fromHex(y))
}
