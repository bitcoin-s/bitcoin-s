package org.bitcoins.crypto

import org.bouncycastle.asn1.sec.SECNamedCurves
import org.bouncycastle.asn1.x9.X9ECParameters
import org.bouncycastle.crypto.params.ECDomainParameters

import java.math.BigInteger

/** Created by chris on 3/29/16.
  * This trait represents all of the default parameters for our elliptic curve
  */
sealed abstract class CryptoParams {

  /** This is the parameters for the elliptic curve bitcoin uses. */
  def params: X9ECParameters = SECNamedCurves.getByName("secp256k1")

  /** The curve that bitcoin uses. */
  def curve =
    new ECDomainParameters(params.getCurve,
                           params.getG,
                           params.getN,
                           params.getH)

  /** This is used for canonicalising the S value of a digital signature.
    * https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#low-s-values-in-signatures
    * @return
    */
  def halfCurveOrder: BigInteger = curve.getN.shiftRight(1)

}

object CryptoParams extends CryptoParams
