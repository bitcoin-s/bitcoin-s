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

}
