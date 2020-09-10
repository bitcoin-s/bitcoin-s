package org.bitcoins.core.wallet.keymanagement

sealed trait KeyManagerUnlockError extends Error

object KeyManagerUnlockError {

  final case object MnemonicNotFound
      extends Error("Mnemonic not found")
      with KeyManagerUnlockError

  final case object BadPassword
      extends Error("Bad password for unlocking wallet!")
      with KeyManagerUnlockError

  final case class JsonParsingError(message: String)
      extends Error(message)
      with KeyManagerUnlockError
}
