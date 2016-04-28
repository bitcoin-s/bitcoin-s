package org.bitcoins.crypto

import org.spongycastle.asn1.sec.SECNamedCurves
import org.spongycastle.crypto.params.ECDomainParameters

/**
 * Created by chris on 3/29/16.
 * This trait represents all of the default parameters for our elliptic curve
 */
trait CryptoParams {

  /**
   * This is the parameters for the elliptic curve bitcoin uses
   * @return
   */
  def params = SECNamedCurves.getByName("secp256k1")

  /**
   * The curve that bitcoin uses
   * @return
   */
  def curve = new ECDomainParameters(params.getCurve(), params.getG(), params.getN(),
    params.getH())


  /**
   * This is used for canonicalising the S value of a digital signature.
   * https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#low-s-values-in-signatures
   * @return
   */
  def halfCurveOrder = curve.getN.shiftRight(1)

}

object CryptoParams extends CryptoParams
