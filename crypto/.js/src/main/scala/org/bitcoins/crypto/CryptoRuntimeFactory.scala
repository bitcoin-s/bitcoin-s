package org.bitcoins.crypto

sealed protected trait CryptoRuntimeFactory {
  def newCryptoRuntime: CryptoRuntime = BCryptoCryptoRuntime
}
