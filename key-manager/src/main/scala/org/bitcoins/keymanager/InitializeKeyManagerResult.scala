package org.bitcoins.keymanager

sealed trait InitializeKeyManagerResult

final case class InitializeKeyManagerSuccess(keyManager: KeyManager)
    extends InitializeKeyManagerResult

sealed trait InitializeKeyManagerError
    extends Error
    with InitializeKeyManagerResult

object InitializeKeyManagerError {
  // todo add explanation of what good/bad entropy is
  final case object BadEntropy
      extends Error("Bad Entropy")
      with InitializeKeyManagerError

  final case class EncryptionError(underlying: Throwable)
      extends Error(underlying)
      with InitializeKeyManagerError

  /**
    * Wallet data was found in the
    * specified data directory.
    *
    * @note `bitcoin-s` only supports one wallet
    *   per network at the moment.
    */
  final case object WalletAlreadyExists
      extends Error("Wallet already exists")
      with InitializeKeyManagerError

}
