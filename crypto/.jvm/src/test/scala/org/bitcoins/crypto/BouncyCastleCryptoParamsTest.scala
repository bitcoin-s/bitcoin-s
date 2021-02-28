package org.bitcoins.crypto

import org.scalatest.flatspec.AnyFlatSpec

class BouncyCastleCryptoParamsTest extends AnyFlatSpec {

  behavior of "BouncyCastleCryptoParams"

  it must "have the same CryptoParams.getN & BouncyCastleCryptoParams.getN" in {
    assert(CryptoParams.getN == BouncyCastleCryptoParams.curve.getN)
  }
}
