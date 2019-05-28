package org.bitcoins.wallet.api

sealed trait InitializeWalletResult

final case class InitializeWalletSuccess(wallet: UnlockedWalletApi)
    extends InitializeWalletResult

sealed trait InitializeWalletError extends Error with InitializeWalletResult

object InitializeWalletError {
  // todo add explanation of what good/bad entropy is
  final case object BadEntropy
      extends Error("Bad Entropy")
      with InitializeWalletError

  final case class EncryptionError(underlying: Throwable)
      extends Error(underlying)
      with InitializeWalletError

  /**
    * Wallet data was found in the
    * specified data directory.
    *
    * @note `bitcoin-s` only supports one wallet
    *   per network at the moment.
    */
  final case object WalletAlreadyExists
      extends Error("Wallet already exists")
      with InitializeWalletError

}
