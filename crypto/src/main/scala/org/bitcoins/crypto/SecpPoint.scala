package org.bitcoins.crypto

import scodec.bits.ByteVector

import java.math.BigInteger

/** Represents a point on secp256k1 elliptic curve.
  */
sealed trait SecpPoint

/** The infinity point.
  */
case object SecpPointInfinity extends SecpPoint

/** A point on an elliptic curve.
  * @param x
  * @param y
  */
case class SecpPointImpl(x: FieldElement, y: FieldElement) extends SecpPoint

object SecpPoint {

  def apply(x: ByteVector, y: ByteVector): SecpPoint =
    SecpPointImpl(FieldElement.fromBytes(x), FieldElement.fromBytes(y))

  def apply(x: Array[Byte], y: Array[Byte]): SecpPoint =
    SecpPointImpl(FieldElement.fromByteArray(x), FieldElement.fromByteArray(y))

  def apply(x: BigInteger, y: BigInteger): SecpPoint =
    SecpPointImpl(FieldElement(x), FieldElement(y))

  def apply(x: BigInt, y: BigInt): SecpPoint =
    SecpPointImpl(FieldElement(x), FieldElement(y))

  def apply(x: String, y: String): SecpPoint =
    SecpPointImpl(FieldElement.fromHex(x), FieldElement.fromHex(y))
}
