package org.bitcoins.crypto

/** A trait that is extended by other traits and classes
  * that required access to crypto operations
  * This should be extended because this will provide a
  * platform specific crypto runtime rather than
  * a hardcoded one for the JVM.
  */
trait CryptoTrait {
  final lazy val cryptoRuntime: CryptoRuntime = CryptoContext.cryptoRuntime
}
