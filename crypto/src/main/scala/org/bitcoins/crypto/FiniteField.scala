package org.bitcoins.crypto

import scodec.bits.ByteVector

import java.math.BigInteger

abstract class FiniteFieldMember[F <: FiniteFieldMember[F]](
    fieldOrder: BigInteger,
    byteSize: Int)
    extends NetworkElement {
  require(bytes.length == byteSize,
          s"Finite field member must have $byteSize bytes, got $bytes")
  require(
    toBigInteger.compareTo(fieldOrder) < 0,
    s"$bytes is not a valid field member (was not less than $fieldOrder).")

  def isZero: Boolean = bytes.toArray.forall(_ == 0.toByte)

  def isEven: Boolean = {
    (bytes.last & 0x01) == 0
  }

  def isOdd: Boolean = !isEven

  def fieldObj: FiniteFieldObject[F]

  def thisAsF: F = this.asInstanceOf[F]

  def toBigInteger: BigInteger = fieldObj.getBigInteger(bytes)

  def add(other: F): F = {
    fieldObj.add(thisAsF, other)
  }

  def subtract(other: F): F = {
    add(other.negate)
  }

  def multiply(other: F): F = {
    fieldObj.multiply(thisAsF, other)
  }

  def multInv(other: F): F = {
    multiply(other.inverse)
  }

  def negate: F = {
    fieldObj.negate(thisAsF)
  }

  def inverse: F = fieldObj.computeInverse(thisAsF)
}

abstract class FiniteFieldObject[F <: FiniteFieldMember[F]](
    fieldOrder: BigInteger,
    byteSize: Int)
    extends Factory[F] {

  def fieldMemberConstructor(bytes: ByteVector): F

  override def fromBytes(bytes: ByteVector): F = {
    if (bytes.length < byteSize) {
      fieldMemberConstructor(bytes.padLeft(byteSize))
    } else if (bytes.length == byteSize) {
      fieldMemberConstructor(bytes)
    } else if (bytes.length == byteSize + 1 && bytes.head == 0.toByte) {
      fieldMemberConstructor(bytes.tail)
    } else {
      throw new IllegalArgumentException(
        s"Field element cannot have more than 32 bytes, got $bytes")
    }
  }

  def apply(num: BigInt): F = {
    apply(num.underlying())
  }

  def apply(num: BigInteger): F = {
    fromByteArray(num.mod(fieldOrder).toByteArray)
  }

  def fromByteArray(bytes: Array[Byte]): F = {
    fromBytes(ByteVector(bytes))
  }

  lazy val zero: F = fromBytes(ByteVector.empty)
  lazy val one: F = fromBytes(ByteVector.fromByte(1))
  lazy val orderMinusOne: F = apply(fieldOrder.subtract(BigInteger.ONE))

  def getBigInteger(bytes: ByteVector): BigInteger = {
    new BigInteger(1, bytes.toArray)
  }

  def add(fe1: F, fe2: F): F = {
    val num1 = fe1.toBigInteger
    val num2 = fe2.toBigInteger

    val sum = num1.add(num2).mod(fieldOrder)
    apply(sum)
  }

  def multiply(fe1: F, fe2: F): F = {
    val num1 = fe1.toBigInteger
    val num2 = fe2.toBigInteger

    val sum = num1.multiply(num2).mod(fieldOrder)
    apply(sum)
  }

  def negate(fe: F): F = {
    val neg = fieldOrder.subtract(fe.toBigInteger)
    apply(neg)
  }

  /** Computes the inverse (mod fieldOrder) of the input using the Euclidean Algorithm (log time)
    * Cribbed from [[https://www.geeksforgeeks.org/multiplicative-inverse-under-modulo-m/]]
    */
  def computeInverse(fe: F): F = {
    val inv = fe.toBigInteger.modInverse(fieldOrder)
    apply(inv)
  }
}
