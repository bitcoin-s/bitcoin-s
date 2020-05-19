package org.bitcoins.crypto

import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}

/**
  * Created by chris on 3/23/16.
  */
sealed abstract class DERSignatureUtil {

  /**
    * Checks if this signature is encoded to DER correctly
    * https://crypto.stackexchange.com/questions/1795/how-can-i-convert-a-der-ecdsa-signature-to-asn-1
    * NOTE: This will fail if this signature contains the hash type appended to the end of it
    * @return boolean representing if the signature is a valid
    */
  def isDEREncoded(signature: ECDigitalSignature): Boolean =
    isDEREncoded(signature.bytes)

  /**
    * Checks if the bytes are encoded to DER correctly
    * https://crypto.stackexchange.com/questions/1795/how-can-i-convert-a-der-ecdsa-signature-to-asn-1
    * This will fail if this signature contains the hash type appended to the end of it
    * @return boolean representing if the signature is a valid
    */
  def isDEREncoded(bytes: ByteVector): Boolean = {
    //signature is trivially valid if the signature is empty
    if (bytes.nonEmpty && bytes.size < 9) false
    else if (bytes.nonEmpty) {
      //first byte must be 0x30
      val firstByteIs0x30 = bytes.head == 0x30
      //second byte must indicate the length of the remaining byte array
      val signatureSize = bytes(1).toLong
      //checks to see if the signature length is the same as the signatureSize val
      val signatureLengthIsCorrect = signatureSize == bytes
        .slice(2, bytes.size)
        .size
      //third byte must be 0x02
      val thirdByteIs0x02 = bytes(2) == 0x02
      //this is the size of the r value in the signature
      val rSize = bytes(3)

      //this 0x02 separates the r and s value )in the signature
      val second0x02Exists = bytes(rSize + 4) == 0x02

      firstByteIs0x30 && signatureLengthIsCorrect && thirdByteIs0x02 &&
      second0x02Exists
    } else true
  }

  /**
    * Decodes the given digital signature into it's r and s points
    */
  def decodeSignature(signature: ECDigitalSignature): (BigInt, BigInt) =
    decodeSignature(signature.bytes)

  /**
    * Decodes the given sequence of bytes into it's r and s points
    * throws an exception if the given sequence of bytes is not a DER encoded signature
    */
  def decodeSignature(bytes: ByteVector): (BigInt, BigInt) = {
    DERSignatureUtil.parseDERLax(bytes) match {
      case Some((r, s)) => (r, s)
      case None         => (0, 0)
    }
  }

  /**
    * This functions implements the strict der encoding rules that were created in BIP66
    * [[https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki]]
    * [[https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L98]]
    * @param signature the signature to check if they are strictly der encoded
    * @return boolean indicating whether the signature was der encoded or not
    */
  def isValidSignatureEncoding(signature: ECDigitalSignature): Boolean = {
    signature match {
      case EmptyDigitalSignature => true
      case signature: ECDigitalSignature =>
        isValidSignatureEncoding(signature.bytes)
    }
  }

  /**
    * This functions implements the strict der encoding rules that were created in BIP66
    * https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki
    * [[https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L98]]
    * @param bytes the bytes to check if they are strictly der encoded
    * @return boolean indicating whether the bytes were der encoded or not
    */
  def isValidSignatureEncoding(bytes: ByteVector): Boolean = {
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

    if (bytes.size < 9) return false
    //logger.debug("signature is the minimum size for strict der encoding")
    if (bytes.size > 73) return false
    //logger.debug("signature is under the maximum size for strict der encoding")

    // A signature is of type 0x30 (compound)
    if (bytes.head != 0x30) return false
    //logger.debug("First  byte is 0x30")

    // Make sure the length covers the entire signature.
    if (bytes(1) != bytes.size - 3) return false
    //logger.debug("Signature length covers the entire signature")

    val rSize = bytes(3)
    //logger.debug("rSize: " + rSize)

    // Make sure the length of the S element is still inside the signature.
    if (5 + rSize >= bytes.size) return false
    //logger.debug("Length of S element is contained in the signature")

    // Extract the length of the S element.
    val sSize = bytes(5 + rSize)
    //logger.debug("sSize: " + sSize)

    // Verify that the length of the signature matches the sum of the length
    // of the elements.
    if ((rSize + sSize + 7) != bytes.size) return false
    //logger.debug("Verify that the length of the signature matches the sum of the length of the elements.")

    // Check whether the R element is an integer.
    if (bytes(2) != 0x02) return false
    //logger.debug("R element is an integer")

    // Zero-length integers are not allowed for R.
    if (rSize == 0) return false
    //logger.debug("r is not a zero length integer")

    // Negative numbers are not allowed for R.
    if ((bytes(4) & 0x80) != 0) return false
    //logger.debug("r is not a negative number")

    // Null bytes at the start of R are not allowed, unless R would
    // otherwise be interpreted as a negative number.
    if (rSize > 1 && (bytes(4) == 0x00) && (bytes(5) & 0x80) == 0)
      return false
    //logger.debug("There were not any null bytes at the start of R")
    // Check whether the S element is an integer.
    if (bytes(rSize + 4) != 0x02) return false
    //logger.debug("The S element is an integer")

    // Zero-length integers are not allowed for S.
    if (sSize == 0) return false
    //logger.debug("S was not a zero length integer")

    // Negative numbers are not allowed for S.
    if ((bytes(rSize + 6) & 0x80) != 0) return false
    //logger.debug("s was not a negative number")
    // Null bytes at the start of S are not allowed, unless S would otherwise be
    // interpreted as a negative number.
    if (sSize > 1 && (bytes(rSize + 6) == 0x00) && (bytes(rSize + 7) & 0x80) == 0)
      return false
    //logger.debug("There were not any null bytes at the start of S")
    //if we made it to this point without returning false this must be a valid strictly encoded der sig
    true
  }

  /**
    * Requires the S value in signatures to be the low version of the S value
    * https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#low-s-values-in-signatures
    * @return if the S value is the low version
    */
  def isLowS(signature: ECDigitalSignature): Boolean = isLowS(signature.bytes)

  /**
    * Requires the S value in signatures to be the low version of the S value
    * https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#low-s-values-in-signatures
    * @return if the S value is the low version
    */
  def isLowS(signature: ByteVector): Boolean = {
    val result = Try {
      val (_, s) = decodeSignature(signature)
      s.bigInteger.compareTo(CryptoParams.halfCurveOrder) <= 0
    }
    result match {
      case Success(bool) => bool
      case Failure(_)    => false
    }
  }

  /** Checks if the given digital signature uses a low s value, if it does not it converts it to a low s value and returns it */
  def lowS(signature: ECDigitalSignature): ECDigitalSignature = {
    val sigLowS =
      if (isLowS(signature)) signature
      else
        ECDigitalSignature(
          signature.r,
          CryptoParams.curve.getN.subtract(signature.s.bigInteger))
    require(DERSignatureUtil.isLowS(sigLowS))
    sigLowS
  }

  /** Scala implementation of https://github.com/bitcoin/bitcoin/blob/master/src/pubkey.cpp#L27 */
  def parseDERLax(input: ByteVector): Option[(BigInt, BigInt)] = {
    /* Sequence tag byte */
    if (input.isEmpty || input.head != 0x30.toByte) {
      return None
    }

    var pos: Int = 1

    // Sequence length bytes
    if (input.length == pos) {
      return None
    }
    var lenByte: Byte = input(pos)
    pos += 1
    if ((lenByte & 0x80) != 0) {
      lenByte = (lenByte - 0x80).toByte
      if (lenByte > input.length - pos) {
        return None
      }
      pos += lenByte
    }

    // Integer tag byte for R
    if (pos == input.length || input(pos) != 0x02.toByte) {
      return None
    }
    pos += 1

    // Integer length for R
    if (pos == input.length) {
      return None
    }
    lenByte = input(pos)
    pos += 1
    var rLen = if ((lenByte & 0x80) != 0) {
      lenByte = (lenByte - 0x80).toByte
      if (lenByte > input.length - pos) {
        return None
      }

      while (lenByte > 0 && input(pos) == 0.toByte) {
        pos += 1
        lenByte = (lenByte - 1).toByte
      }
      if (lenByte >= 4) {
        return None
      }
      var rLen = 0
      while (lenByte > 0) {
        rLen = (rLen << 8) + input(pos)
        pos += 1
        lenByte = (lenByte - 1).toByte
      }
      rLen
    } else {
      lenByte.toInt
    }
    if (rLen > input.length - pos) {
      return None
    }
    var rpos = pos
    pos += rLen

    // Integer tag byte for S
    if (pos == input.length || input(pos) != 0x02.toByte) {
      return None
    }
    pos += 1

    // Integer length for S
    if (pos == input.length) {
      return None
    }
    lenByte = input(pos)
    pos += 1
    var sLen = if ((lenByte & 0x80) != 0) {
      lenByte = (lenByte - 0x80).toByte
      if (lenByte > input.length - pos) {
        return None
      }
      while (lenByte > 0 && input(pos) == 0.toByte) {
        pos += 1
        lenByte = (lenByte - 1).toByte
      }
      if (lenByte >= 4) {
        return None
      }
      var sLen = 0
      while (lenByte > 0) {
        sLen = (sLen << 8) + input(pos)
        pos += 1
        lenByte = (lenByte - 1).toByte
      }
      sLen
    } else {
      lenByte
    }
    if (sLen > input.length - pos) {
      return None
    }
    var spos = pos

    // Ignore leading zeroes in R
    while (rLen > 0 && input(rpos) == 0.toByte) {
      rLen -= 1
      rpos += 1
    }

    var overflow = false
    val tmpSig: Array[Byte] = Array.fill(64)(0.toByte)

    if (rLen > 32) {
      overflow = true
    } else {
      System.arraycopy(input.toArray, rpos, tmpSig, 32 - rLen, rLen)
    }

    // Ignore leading zeroes in S
    while (sLen > 0 && input(spos) == 0) {
      sLen -= 1
      spos += 1
    }

    // Copy S value
    if (sLen > 32) {
      overflow = true
    } else {
      System.arraycopy(input.toArray, spos, tmpSig, 64 - sLen, sLen)
    }

    if (overflow) {
      System.arraycopy(Array.fill(64)(0.toByte), 0, tmpSig, 0, 64)
    }

    val r = BigInt(1, tmpSig.take(32))
    val s = BigInt(1, tmpSig.takeRight(32))

    Some((r, s))
  }
}

object DERSignatureUtil extends DERSignatureUtil
