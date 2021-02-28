package org.bitcoins.crypto

import scodec.bits.ByteVector

import java.math.BigInteger

/** Created by chris on 3/29/16.
  * This trait represents all of the default parameters for our elliptic curve
  */
sealed abstract class CryptoParams {

  /** Hex constant for curve group constant
    */
  private val nConstant =
    "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141"

  val getN: BigInteger =
    new BigInteger(1, ByteVector.fromValidHex(nConstant).toArray)

  /** This is used for canonicalising the S value of a digital signature.
    * https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#low-s-values-in-signatures
    * @return
    */
  val halfCurveOrder: BigInteger = getN.shiftRight(1)
}

object CryptoParams extends CryptoParams
