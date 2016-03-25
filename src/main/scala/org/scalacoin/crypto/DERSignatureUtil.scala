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

  /**
   * This functions implements the strict der encoding rules that were created in BIP66
   * https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki
   * @param signature the signature to check if they are strictly der encoded
   * @return boolean indicating whether the signature was der encoded or not
   */
  def isStrictDEREncoding(signature : ECDigitalSignature) : Boolean = isStrictDEREncoding(signature.bytes)


  /**
   * This functions implements the strict der encoding rules that were created in BIP66
   * https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki
   * @param bytes the bytes to check if they are strictly der encoded
   * @return boolean indicating whether the bytes were der encoded or not
   */
  def isStrictDEREncoding(bytes : Seq[Byte]) : Boolean = {
    // Format: 0x30 [total-length] 0x02 [R-length] [R] 0x02 [S-length] [S] [sighash]
    // * total-length: 1-byte length descriptor of everything that follows,
    //   excluding the sighash byte.
    // * R-length: 1-byte length descriptor of the R value that follows.
    // * R: arbitrary-length big-endian encoded R value. It must use the shortest
    //   possible encoding for a positive integers (which means no null bytes at
    //   the start, except a single one when the next byte has its highest bit set).
    // * S-length: 1-byte length descriptor of the S value that follows.
    // * S: arbitrary-length big-endian encoded S value. The same rules apply.
    // * sighash: 1-byte value indicating what data is hashed (not part of the DER
    //   signature)

    //there is a caveat here that this function is trivially true if the
    //empty signature is given to us, aka 0 bytes.
    if (bytes.size == 0) return true

    //check if the bytes are ATLEAST der encoded
    val isDerEncoded = isDEREncoded(bytes)
    if (!isDerEncoded) return false


    if (bytes.size < 9) return false
    if (bytes.size > 73) return false

    // A signature is of type 0x30 (compound)
    if (bytes.head != 0x30) return false

    // Make sure the length covers the entire signature.
    if (bytes(1) != bytes.size - 3) return false

    val rSize = bytes(3)

    // Make sure the length of the S element is still inside the signature.
    if (5 + rSize >= bytes.size) return false

    // Extract the length of the S element.
    val sSize = bytes(5 + rSize)

    // Verify that the length of the signature matches the sum of the length
    // of the elements.
    if ((rSize + sSize + 7) != bytes.size) return false

    // Check whether the R element is an integer.
    if (bytes(2) != 0x02) return false

    // Zero-length integers are not allowed for R.
    if (rSize == 0) return false

    // Negative numbers are not allowed for R.
    if ((bytes(4) & 0x80) != 0) return false

    // Null bytes at the start of R are not allowed, unless R would
    // otherwise be interpreted as a negative number.
    if (rSize > 1 && (bytes(4) == 0x00) && !((bytes(5) & 0x80) != 0 )) return false

    // Check whether the S element is an integer.
    if (bytes(rSize + 4) != 0x02) return false

    // Zero-length integers are not allowed for S.
    if (rSize == 0) return false

    // Negative numbers are not allowed for S.
    if ((bytes(rSize + 6) & 0x80) != 0) return false

    // Null bytes at the start of S are not allowed, unless S would otherwise be
    // interpreted as a negative number.
    if (sSize > 1 && (bytes(rSize + 6) == 0x00) && !((bytes(rSize + 7) & 0x80) != 0)) return false

    //if we made it to this point without returning false this must be a valid strictly encoded der sig
    true
  }
}

object DERSignatureUtil extends DERSignatureUtil
