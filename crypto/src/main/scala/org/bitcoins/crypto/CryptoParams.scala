package org.bitcoins.crypto

import scodec.bits.ByteVector

import java.math.BigInteger

/** Created by chris on 3/29/16.
  * This trait represents all of the default parameters for our elliptic curve
  */
sealed abstract class CryptoParams {

  private val curvePrimeConstant =
    "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F"

  val getCurvePrime: BigInteger =
    new BigInteger(1, ByteVector.fromValidHex(curvePrimeConstant).toArray)

  /** Hex constant for curve group constant
    */
  private val nConstant =
    "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141"

  val getN: BigInteger =
    new BigInteger(1, ByteVector.fromValidHex(nConstant).toArray)

  private val gDecompressedBytes =
    "0479be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8"

  // This needs to be lazy otherwise this class fails to initialize
  lazy val getG: ECPublicKey = ECPublicKey(gDecompressedBytes)

  /** This is used for canonicalising the S value of a digital signature.
    * https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#low-s-values-in-signatures
    * @return
    */
  val halfCurveOrder: BigInteger = getN.shiftRight(1)
}

object CryptoParams extends CryptoParams
