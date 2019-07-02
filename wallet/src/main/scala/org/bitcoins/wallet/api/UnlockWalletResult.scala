package org.bitcoins.wallet.api

sealed trait UnlockWalletResult

final case class UnlockWalletSuccess(unlockedWalletApi: UnlockedWalletApi)
    extends UnlockWalletResult

sealed trait UnlockWalletError extends Error with UnlockWalletResult

object UnlockWalletError {

  final case object MnemonicNotFound
      extends Error("Mnemonic nout found")
      with UnlockWalletError

  final case object BadPassword
      extends Error("Bad password for unlocking wallet!")
      with UnlockWalletError

  final case class JsonParsingError(message: String)
      extends Error(message)
      with UnlockWalletError
}
