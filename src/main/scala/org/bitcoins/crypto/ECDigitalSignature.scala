package org.bitcoins.crypto

import org.bitcoins.util.{BitcoinSLogger, BitcoinSUtil}

/**
 * Created by chris on 2/26/16.
 */
sealed trait ECDigitalSignature extends BitcoinSLogger {

  def hex : String = BitcoinSUtil.encodeHex(bytes)
  def bytes : Seq[Byte]
  def isEmpty = bytes.isEmpty
  override def toString = hex


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
sealed case class ECDigitalSignatureImpl(bytes : Seq[Byte]) extends ECDigitalSignature
