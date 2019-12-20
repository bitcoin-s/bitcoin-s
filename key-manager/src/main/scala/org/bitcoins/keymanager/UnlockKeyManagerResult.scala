package org.bitcoins.keymanager

sealed trait UnlockKeyManagerError extends Error

object UnlockKeyManagerError {

  final case object MnemonicNotFound
      extends Error("Mnemonic not found")
      with UnlockKeyManagerError

  final case object BadPassword
      extends Error("Bad password for unlocking wallet!")
      with UnlockKeyManagerError

  final case class JsonParsingError(message: String)
      extends Error(message)
      with UnlockKeyManagerError
}
