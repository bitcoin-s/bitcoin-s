package org.bitcoins.keymanager

import java.nio.file.Path

import org.bitcoins.core.crypto.AesPassword
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.keymanager.ReadMnemonicError.{
  DecryptionError,
  JsonParsingError
}

/** Represents a  */
object LockedKeyManager extends BitcoinSLogger {

  /**
    * Unlock the wallet by decrypting the [[EncryptedMnemonic]] seed
    * @param passphrase the password to decrypt the wallet
    * @param seedPath where the seed is located
    */
  def unlock(
      passphrase: AesPassword,
      seedPath: Path): UnlockKeyManagerResult = {
    logger.debug(s"Trying to unlock wallet with seedPath=${seedPath}")
    val result =
      WalletStorage.decryptMnemonicFromDisk(seedPath, passphrase)
    result match {
      case DecryptionError =>
        logger.error(s"Bad password for unlocking wallet!")
        UnlockKeyManagerError.BadPassword
      case JsonParsingError(message) =>
        logger.error(s"JSON parsing error when unlocking wallet: $message")
        UnlockKeyManagerError.JsonParsingError(message)
      case ReadMnemonicError.NotFoundError =>
        logger.error(s"Encrypted mnemonic not found when unlocking the wallet!")
        UnlockKeyManagerError.MnemonicNotFound

      case ReadMnemonicSuccess(mnemonic) =>
        logger.debug(s"Successfully unlocked wallet")
        UnlockKeyManagerSuccess(KeyManager(mnemonic))
    }
  }
}
