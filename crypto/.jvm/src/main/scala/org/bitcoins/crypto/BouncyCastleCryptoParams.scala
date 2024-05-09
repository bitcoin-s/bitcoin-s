package org.bitcoins.crypto

import org.bouncycastle.asn1.sec.SECNamedCurves
import org.bouncycastle.asn1.x9.X9ECParameters
import org.bouncycastle.crypto.params.ECDomainParameters

object BouncyCastleCryptoParams {
  val params: X9ECParameters = SECNamedCurves.getByName("secp256k1")

  /** The curve that bitcoin uses. */
  val curve =
    new ECDomainParameters(params.getCurve,
                           params.getG,
                           params.getN,
                           params.getH)
}
