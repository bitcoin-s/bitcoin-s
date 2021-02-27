package org.bitcoins.crypto

import scodec.bits.ByteVector

import java.math.BigInteger
import scala.util.Try

/** Represents integers modulo the secp256k1 field size: pow(2,256) - 0x1000003D1.
  *
  * Supports arithmetic for these elements including +, -, *, and inverses.
  * Supports 32 byte serialization as is needed for ECPrivateKeys.
  */
case class FieldElement(bytes: ByteVector) extends NetworkElement {
  require(bytes.length == 32, s"Field elements must have 32 bytes, got $bytes")

  private val privKeyT: Try[ECPrivateKey] = Try(ECPrivateKey(bytes))

  require(
    privKeyT.isSuccess || isZero,
    s"$bytes is not a valid field element: ${privKeyT.failed.get.getMessage}")

  def isZero: Boolean = bytes.toArray.forall(_ == 0.toByte)

  def toPrivateKey: ECPrivateKey =
    if (!isZero) {
      privKeyT.get
    } else {
      throw new RuntimeException("Cannot turn zero into a private key")
    }

  def toBigInteger: BigInteger = FieldElement.getBigInteger(bytes)

  def add(other: FieldElement): FieldElement = {
    FieldElement.add(this, other)
  }

  def subtract(other: FieldElement): FieldElement = {
    add(other.negate)
  }

  def multiply(other: FieldElement): FieldElement = {
    FieldElement.multiply(this, other)
  }

  def multInv(other: FieldElement): FieldElement = {
    multiply(other.inverse)
  }

  def negate: FieldElement = {
    FieldElement.negate(this)
  }

  def getPublicKey: ECPublicKey = toPrivateKey.publicKey

  def inverse: FieldElement = FieldElement.computeInverse(this)
}

object FieldElement extends Factory[FieldElement] {

  override def fromBytes(bytes: ByteVector): FieldElement = {
    if (bytes.length < 32) {
      new FieldElement(bytes.padLeft(32))
    } else if (bytes.length == 32) {
      new FieldElement(bytes)
    } else if (bytes.length == 33 && bytes.head == 0.toByte) {
      new FieldElement(bytes.tail)
    } else {
      throw new IllegalArgumentException(
        s"Field element cannot have more than 32 bytes, got $bytes")
    }
  }

  def apply(num: BigInt): FieldElement = {
    FieldElement(num.underlying())
  }

  def apply(num: BigInteger): FieldElement = {
    FieldElement.fromByteArray(num.mod(N).toByteArray)
  }

  def fromByteArray(bytes: Array[Byte]): FieldElement = {
    FieldElement(ByteVector(bytes))
  }

  val zero: FieldElement = FieldElement(ByteVector.empty)
  val one: FieldElement = FieldElement(ByteVector.fromByte(1))

  val nMinusOne: FieldElement = FieldElement(
    "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140")

  // CryptoParams.curve.getG
  private val G: ECPublicKey = ECPublicKey(
    "0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798")

  // CryptoParams.curve.getN
  private val N: BigInteger = new BigInteger(
    "115792089237316195423570985008687907852837564279074904382605163141518161494337")

  private def getBigInteger(bytes: ByteVector): BigInteger = {
    new BigInteger(1, bytes.toArray)
  }

  def add(fe1: FieldElement, fe2: FieldElement): FieldElement = {
    val num1 = fe1.toBigInteger
    val num2 = fe2.toBigInteger

    val sum = num1.add(num2).mod(N)
    FieldElement(sum)
  }

  def multiply(fe1: FieldElement, fe2: FieldElement): FieldElement = {
    val num1 = fe1.toBigInteger
    val num2 = fe2.toBigInteger

    val sum = num1.multiply(num2).mod(N)
    FieldElement(sum)
  }

  def negate(fe: FieldElement): FieldElement = {
    val neg = N.subtract(fe.toBigInteger)
    FieldElement(neg)
  }

  def computePoint(fe: FieldElement): ECPublicKey = G.tweakMultiply(fe)

  /** Computes the inverse (mod M) of the input using the Euclidean Algorithm (log time)
    * Cribbed from [[https://www.geeksforgeeks.org/multiplicative-inverse-under-modulo-m/]]
    */
  def computeInverse(fe: FieldElement): FieldElement = {
    val inv = fe.toBigInteger.modInverse(N)
    FieldElement(inv)
  }
}
