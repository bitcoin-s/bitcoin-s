package org.bitcoins.crypto

sealed trait CryptoContext

object CryptoContext {

  //case object LibSecp256k1 extends CryptoContext

  case object BouncyCastle extends CryptoContext

  /** Only bouncy castle supported for scala/scalajs */
  def default: CryptoContext = {
    //val secpDisabled = System.getenv("DISABLE_SECP256K1")
    BouncyCastle
  }
}
