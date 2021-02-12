package org.bitcoins.crypto

import org.bitcoin.Secp256k1Context

sealed trait CryptoContext

object CryptoContext {

  case object LibSecp256k1 extends CryptoContext

  case object BouncyCastle extends CryptoContext

  def default: CryptoContext = {
    val secpDisabled = System.getenv("DISABLE_SECP256K1")
    if (
      secpDisabled != null && (secpDisabled.toLowerCase == "true" || secpDisabled == "1")
    ) {
      BouncyCastle
    } else {
      if (Secp256k1Context.isEnabled) {
        LibSecp256k1
      } else {
        BouncyCastle
      }
    }
  }

  /** The platform specific cryptographic functions required to run bitcoin-s */
  lazy val cryptoRuntime: CryptoRuntime = {
    default match {
      case LibSecp256k1 => JvmCryptoRuntime
      case BouncyCastle => JvmCryptoRuntime
    }
  }
}
