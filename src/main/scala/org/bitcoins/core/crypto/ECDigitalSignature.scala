package org.bitcoins.core.crypto

import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, Factory}
/**
 * Created by chris on 2/26/16.
 */
sealed trait ECDigitalSignature extends BitcoinSLogger {

  def hex : String = BitcoinSUtil.encodeHex(bytes)

  def bytes : Seq[Byte]

  def isEmpty = bytes.isEmpty

  override def toString = "ECDigitalSignature(" + hex + ")"

  /**
   * Checks if this signature is encoded to DER correctly
   * https://crypto.stackexchange.com/questions/1795/how-can-i-convert-a-der-ecdsa-signature-to-asn-1
 *
   * @return boolean representing if the signature is a valid
   */
  def isDEREncoded : Boolean = DERSignatureUtil.isDEREncoded(this)


  /**
   * Decodes the digital signature into it's r and s points
   * throws an exception if the given sequence of bytes is not a DER encoded signature
 *
   * @return the (r,s) values for the elliptic curve digital signature
   */
  def decodeSignature : (BigInt,BigInt) = DERSignatureUtil.decodeSignature(this)


  /**
   * Represents the r value found in a elliptic curve digital signature
   */
  def r = decodeSignature._1


  /**
   * Represents the s value found in a elliptic curve digital signature
 *
   * @return
   */
  def s = decodeSignature._2

}

case object EmptyDigitalSignature extends ECDigitalSignature {
  def bytes = Seq()
  override def r = java.math.BigInteger.valueOf(0)
  override def s = r
}


object ECDigitalSignature extends Factory[ECDigitalSignature] {
  private case class ECDigitalSignatureImpl(bytes : Seq[Byte]) extends ECDigitalSignature

  override def fromBytes(bytes : Seq[Byte]) : ECDigitalSignature = {
    //this represents the empty signature
    if (bytes.size == 1 && bytes.head == 0x0) EmptyDigitalSignature
    else if (bytes.size == 0) EmptyDigitalSignature
    else ECDigitalSignatureImpl(bytes)
  }

  def apply(r : BigInt, s : BigInt) = fromRS(r,s)
  /**
    * Takes in the r and s component of a digital signature and gives back a ECDigitalSignature object
    * The ECDigitalSignature object complies with strict der encoding as per BIP62
    * note: That the hash type for the signature CANNOT be added to the digital signature
    * @param r the r component of the digital signature
    * @param s the s component of the digital signature
    * @return
    */
  def fromRS(r : BigInt, s : BigInt) : ECDigitalSignature = {
    val rsSize = r.toByteArray.size + s.toByteArray.size
    val totalSize = 4 + rsSize
    val bytes : Seq[Byte] = Seq(0x30.toByte, totalSize.toByte, 0x2.toByte, r.toByteArray.size.toByte) ++
      r.toByteArray.toSeq ++ Seq(0x2.toByte, s.toByteArray.size.toByte) ++ s.toByteArray.toSeq
    fromBytes(bytes)
  }
}
