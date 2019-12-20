package org.bitcoins.keymanager

sealed trait InitializeKeyManagerError extends Error

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

  final case class FailedToReadWrittenSeed(unlockErr: UnlockKeyManagerError)
      extends Error(s"We failed to read the mnemonic seed we just wrote")
      with InitializeKeyManagerError
}
