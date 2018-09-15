package org.bitcoins.core.crypto

import org.bitcoins.core.util.{ BitcoinSLogger, BitcoinSUtil, Factory }
import scodec.bits.ByteVector
/**
 * Created by chris on 2/26/16.
 */
sealed abstract class ECDigitalSignature {
  private val logger = BitcoinSLogger.logger

  def hex: String = BitcoinSUtil.encodeHex(bytes)

  def bytes: ByteVector

  def isEmpty = bytes.isEmpty

  override def toString = "ECDigitalSignature(" + hex + ")"

  /**
   * Checks if this signature is encoded to DER correctly
   * https://crypto.stackexchange.com/questions/1795/how-can-i-convert-a-der-ecdsa-signature-to-asn-1
   * @return boolean representing if the signature is a valid
   */
  def isDEREncoded: Boolean = DERSignatureUtil.isDEREncoded(this)

  /**
   * Checks if the signature is strictly der encoded as per BIP66
   * [[https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki]]
   */
  def isStrictEncoded: Boolean = DERSignatureUtil.isValidSignatureEncoding(this)

  /**
   * Decodes the digital signature into it's r and s points
   * throws an exception if the given sequence of bytes is not a DER encoded signature
   * @return the (r,s) values for the elliptic curve digital signature
   */
  def decodeSignature: (BigInt, BigInt) = DERSignatureUtil.decodeSignature(this)

  /** Represents the r value found in a elliptic curve digital signature */
  def r: BigInt = decodeSignature._1

  /** Represents the s value found in a elliptic curve digital signature */
  def s: BigInt = decodeSignature._2

  /**
   * Creates a ByteVector with only
   * the 32byte r value and 32 byte s value
   * in the vector
   */
  def toRawRS: ByteVector = {

    //comeback and look at this, i think this will fail if r/s is a small bigint
    val rBytes = r.toByteArray.takeRight(32)
    val sBytes = s.toByteArray.takeRight(32)

    val rPadded = ByteVector(rBytes) //.padLeft(32)
    val sPadded = ByteVector(sBytes) //.padLeft(32)

    require(rPadded.size == 32, s"rPadded.size ${rPadded.size}")
    require(sPadded.size == 32, s"sPadded.size ${sPadded.size}")
    rPadded ++ sPadded
  }

}

case object EmptyDigitalSignature extends ECDigitalSignature {
  override val bytes = ByteVector.empty
  override def r = java.math.BigInteger.valueOf(0)
  override def s = r
}

/**
 * The point of this case object is to help with fee estimation
 * an average [[ECDigitalSignature]] is 72 bytes in size
 * Technically this number can vary, 72 bytes is the most
 * likely though according to
 * https://en.bitcoin.it/wiki/Elliptic_Curve_Digital_Signature_Algorithm
 */
case object DummyECDigitalSignature extends ECDigitalSignature {
  override val bytes = ByteVector(Array.fill(72)(0.toByte))
}

object ECDigitalSignature extends Factory[ECDigitalSignature] {
  private case class ECDigitalSignatureImpl(bytes: ByteVector) extends ECDigitalSignature

  override def fromBytes(bytes: ByteVector): ECDigitalSignature = {
    //this represents the empty signature
    if (bytes.size == 1 && bytes.head == 0x0) EmptyDigitalSignature
    else if (bytes.size == 0) EmptyDigitalSignature
    else {
      //make sure the signature follows BIP62's low-s value
      //https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#Low_S_values_in_signatures
      //bitcoinj implementation
      //https://github.com/bitcoinj/bitcoinj/blob/1e66b9a8e38d9ad425507bf5f34d64c5d3d23bb8/core/src/main/java/org/bitcoinj/core/ECKey.java#L551
      ECDigitalSignatureImpl(bytes)
    }
  }

  def apply(r: BigInt, s: BigInt) = fromRS(r, s)
  /**
   * Takes in the r and s component of a digital signature and gives back a ECDigitalSignature object
   * The ECDigitalSignature object complies with strict der encoding as per BIP62
   * note: That the hash type for the signature CANNOT be added to the digital signature
   *
   * @param r the r component of the digital signature
   * @param s the s component of the digital signature
   * @return
   */
  def fromRS(r: BigInt, s: BigInt): ECDigitalSignature = {
    val rsSize = r.toByteArray.size + s.toByteArray.size
    val totalSize = 4 + rsSize
    val bytes: ByteVector = {
      ByteVector(Array(0x30.toByte, totalSize.toByte, 0x2.toByte, r.toByteArray.size.toByte))
        .++(ByteVector(r.toByteArray))
        .++(ByteVector(Array(0x2.toByte, s.toByteArray.size.toByte)))
        .++(ByteVector(s.toByteArray))
    }

    fromBytes(bytes)
  }

  /**
   * Reads a 64 byte bytevector and assumes
   * the first 32 bytes in the R value,
   * the second 32 is the value
   */
  def fromRS(byteVector: ByteVector): ECDigitalSignature = {
    require(
      byteVector.length == 64,
      s"Incorrect size for reading a ECDigital signature from a bytevec, got ${byteVector.length}")
    val r = BigInt(byteVector.take(32).toArray)
    val s = BigInt(byteVector.takeRight(32).toArray)
    fromRS(r, s)
  }

  /**
   * Reads a 64 byte bytevector and assumes
   * the first 32 bytes in the R value,
   * the second 32 is the value
   */
  def fromRS(hex: String): ECDigitalSignature = {
    val bytes = BitcoinSUtil.decodeHex(hex)
    fromRS(bytes)
  }
}
