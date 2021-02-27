package org.bitcoins.crypto

sealed trait CryptoContext

object CryptoContext {

  case object LibSecp256k1 extends CryptoContext

  case object BouncyCastle extends CryptoContext

  case object BCrypto extends CryptoContext

  lazy val cryptoRuntime: CryptoRuntime = CryptoRuntimeFactory.newCryptoRuntime

  lazy val default: CryptoContext = cryptoRuntime.cryptoContext

}
