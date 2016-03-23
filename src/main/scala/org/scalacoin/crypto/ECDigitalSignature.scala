package org.scalacoin.crypto

import org.scalacoin.util.{BitcoinSLogger, BitcoinSUtil}

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
   * @return boolean representing if the signature is a valid
   */
  def isDEREncoded : Boolean = isDEREncoded(this)


  /**
   * Checks if this signature is encoded to DER correctly
   * https://crypto.stackexchange.com/questions/1795/how-can-i-convert-a-der-ecdsa-signature-to-asn-1
   * @return boolean representing if the signature is a valid
   */
  def isDEREncoded(signature : ECDigitalSignature) : Boolean = {
    //signature is trivially valid if the signature is empty
    if (!signature.isEmpty) {
      //first byte must be 0x30
      val firstByteIs0x30 = signature.bytes.head == 0x30
      logger.debug("firstByteIs0x30: " + firstByteIs0x30)
      //second byte must indicate the length of the remaining byte array
      val signatureSize = signature.bytes(1).toLong
      logger.debug("Encoded Signature Size: " + signatureSize)
      logger.debug("Actual Signature Size: " + bytes.slice(3,bytes.size).size)
      //checks to see if the signature length is the same as the signatureSize val
      val signatureLengthIsCorrect = signatureSize == bytes.slice(3,bytes.size).size
      logger.debug("Signature length is correct: " + signatureLengthIsCorrect)
      //third byte must be 0x02
      val thirdByteIs0x02 = signature.bytes(2) == 0x02
      logger.debug("Third byte is 0x02: " + thirdByteIs0x02)
      //this is the size of the r value in the signature
      val rSize = signature.bytes(3)
      logger.debug("R size: " + rSize)
      //r value in the signature
      val r = signature.bytes.slice(3,rSize+3)
      logger.debug("R: " + BitcoinSUtil.encodeHex(r))
      //this 0x02 separates the r and s value )in the signature
      val second0x02Exists = signature.bytes(rSize + 4) == 0x02
      logger.debug("Second 0x02 exists: " + second0x02Exists)
      //this is the size of the s value in the signature
      val sSize = signature.bytes(rSize + 4)
      logger.debug("S Size: " + sSize)

      val s = signature.bytes.slice(rSize + 4 + 1, signature.bytes.size)
      logger.debug("S: " + BitcoinSUtil.encodeHex(s))
      firstByteIs0x30 && signatureLengthIsCorrect && thirdByteIs0x02 &&
        second0x02Exists
    } else true

  }
}

sealed case class ECDigitalSignatureImpl(bytes : Seq[Byte]) extends ECDigitalSignature
