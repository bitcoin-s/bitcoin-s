package org.scalacoin.crypto

import org.scalacoin.util.{BitcoinSLogger, BitcoinSUtil}
import org.spongycastle.asn1.{ASN1Primitive, ASN1Integer, DLSequence, ASN1InputStream}

/**
 * Created by chris on 3/23/16.
 */
trait DERSignatureUtil extends BitcoinSLogger {

  /**
   * Checks if this signature is encoded to DER correctly
   * https://crypto.stackexchange.com/questions/1795/how-can-i-convert-a-der-ecdsa-signature-to-asn-1
   * @return boolean representing if the signature is a valid
   */
  def isDEREncoded(signature : ECDigitalSignature) : Boolean = isDEREncoded(signature.bytes)

  /**
   * Checks if the bytes are encoded to DER correctly
   * https://crypto.stackexchange.com/questions/1795/how-can-i-convert-a-der-ecdsa-signature-to-asn-1
   * @return boolean representing if the signature is a valid
   */
  def isDEREncoded(bytes : Seq[Byte]) : Boolean = {
    //signature is trivially valid if the signature is empty
    if (!bytes.isEmpty) {
      //first byte must be 0x30
      val firstByteIs0x30 = bytes.head == 0x30
      logger.debug("firstByteIs0x30: " + firstByteIs0x30)
      //second byte must indicate the length of the remaining byte array
      val signatureSize = bytes(1).toLong
      logger.debug("Encoded Sisgnature Size: " + signatureSize)
      logger.debug("Actual Signature Size: " + bytes.slice(3,bytes.size).size)
      //checks to see if the signature length is the same as the signatureSize val
      val signatureLengthIsCorrect = signatureSize == bytes.slice(3,bytes.size).size
      logger.debug("Signature length is correct: " + signatureLengthIsCorrect)
      //third byte must be 0x02
      val thirdByteIs0x02 = bytes(2) == 0x02
      logger.debug("Third byte is 0x02: " + thirdByteIs0x02)
      //this is the size of the r value in the signature
      val rSize = bytes(3)
      logger.debug("R size: " + rSize)
      //r value in the signature
      val r = bytes.slice(3,rSize+3)
      logger.debug("R: " + BitcoinSUtil.encodeHex(r))
      //this 0x02 separates the r and s value )in the signature
      val second0x02Exists = bytes(rSize + 4) == 0x02
      logger.debug("Second 0x02 exists: " + second0x02Exists)
      //this is the size of the s value in the signature
      val sSize = bytes(rSize + 4)
      logger.debug("S Size: " + sSize)

      val s = bytes.slice(rSize + 4 + 1, bytes.size)
      logger.debug("S: " + BitcoinSUtil.encodeHex(s))
      firstByteIs0x30 && signatureLengthIsCorrect && thirdByteIs0x02 &&
        second0x02Exists
    } else true
  }


  /**
   * Decodes the given digital signature into it's r and s points
   * @param signature
   * @return
   */
  def decodeSignature(signature : ECDigitalSignature) : (BigInt,BigInt) = decodeSignature(signature.bytes)

  /**
   * Decodes the given sequence of bytes into it's r and s points
   * throws an exception if the given sequence of bytes is not a DER encoded signature
   * @param bytes
   * @return
   */
  def decodeSignature(bytes : Seq[Byte]) : (BigInt,BigInt) = {
    if (isDEREncoded(bytes)) {
      val asn1InputStream = new ASN1InputStream(bytes.toArray)
      //TODO: this is nasty, is there any way to get rid of all this casting???
      //https://stackoverflow.com/questions/2409618/how-do-i-decode-a-der-encoded-string-in-java
      val seq : DLSequence = asn1InputStream.readObject.asInstanceOf[DLSequence]
      val r = seq.getObjectAt(0).asInstanceOf[ASN1Integer]
      val s = seq.getObjectAt(1).asInstanceOf[ASN1Integer]
      asn1InputStream.close()
      (r.getPositiveValue, s.getPositiveValue)
    } else throw new RuntimeException("The given sequence of bytes was not a DER signature: " + BitcoinSUtil.encodeHex(bytes))
  }
}

object DERSignatureUtil extends DERSignatureUtil
