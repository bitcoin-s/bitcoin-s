package org.bitcoins.crypto

import org.bitcoin.Secp256k1Context

sealed protected trait CryptoRuntimeFactory {

  def newCryptoRuntime: CryptoRuntime = {
    val secpDisabled = System.getenv("DISABLE_SECP256K1")
    if (
      secpDisabled != null && (secpDisabled.toLowerCase == "true" || secpDisabled == "1")
    ) {
      BouncycastleCryptoRuntime
    } else {
      if (Secp256k1Context.isEnabled) {
        LibSecp256k1CryptoRuntime
      } else {
        BouncycastleCryptoRuntime
      }
    }
  }

}

protected object CryptoRuntimeFactory extends CryptoRuntimeFactory
