package org.scalacoin.crypto

import org.spongycastle.asn1.sec.SECNamedCurves

/**
 * Created by chris on 3/29/16.
 * This trait represents all of the default parameters for our elliptic curve
 */
trait CryptoParams {

  /**
   * This is the elliptic curve bitcoin uses
   * @return
   */
  def params = SECNamedCurves.getByName("secp256k1")
}

object CryptoParams extends CryptoParams
