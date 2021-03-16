package org.bitcoins.crypto

import scodec.bits.ByteVector

import java.math.BigInteger

/** Represents a point on an elliptic curve.
  */
sealed trait ECPoint

/** The infinity point.
  */
case object ECPointInfinity extends ECPoint

/** A point on an elliptic curve.
  * @param x
  * @param y
  */
case class ECPointImpl(x: FieldElement, y: FieldElement) extends ECPoint

object ECPoint {

  def apply(x: ByteVector, y: ByteVector): ECPoint =
    ECPointImpl(FieldElement.fromBytes(x), FieldElement.fromBytes(y))

  def apply(x: Array[Byte], y: Array[Byte]): ECPoint =
    ECPointImpl(FieldElement.fromByteArray(x), FieldElement.fromByteArray(y))

  def apply(x: BigInteger, y: BigInteger): ECPoint =
    ECPointImpl(FieldElement(x), FieldElement(y))

  def apply(x: BigInt, y: BigInt): ECPoint =
    ECPointImpl(FieldElement(x), FieldElement(y))

  def apply(x: String, y: String): ECPoint =
    ECPointImpl(FieldElement.fromHex(x), FieldElement.fromHex(y))
}
