package org.bitcoins.crypto

sealed protected trait CryptoRuntimeFactory {
  def newCryptoRuntime: CryptoRuntime = BCryptoCryptoRuntime
}

protected object CryptoRuntimeFactory extends CryptoRuntimeFactory
