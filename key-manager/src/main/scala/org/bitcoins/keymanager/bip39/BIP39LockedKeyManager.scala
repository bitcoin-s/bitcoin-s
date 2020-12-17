package org.bitcoins.keymanager.bip39

import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.keymanagement.{
  KeyManagerParams,
  KeyManagerUnlockError
}
import org.bitcoins.crypto.AesPassword
import org.bitcoins.keymanager.ReadMnemonicError.{
  DecryptionError,
  JsonParsingError
}
import org.bitcoins.keymanager._

/** Represents a */
object BIP39LockedKeyManager extends BitcoinSLogger {

  /**
    * Unlock the wallet by decrypting the [[EncryptedMnemonic]] seed
    * @param passphrase the password to decrypt the wallet
    * @param kmParams parameters needed to create the key manager
    */
  def unlock(
      passphraseOpt: Option[AesPassword],
      bip39PasswordOpt: Option[String],
      kmParams: KeyManagerParams): Either[
    KeyManagerUnlockError,
    BIP39KeyManager] = {
    logger.debug(s"Trying to unlock wallet with seedPath=${kmParams.seedPath}")
    val resultE =
      WalletStorage.decryptSeedFromDisk(kmParams.seedPath, passphraseOpt)
    resultE match {
      case Right(mnemonic: DecryptedMnemonic) =>
        Right(
          BIP39KeyManager.fromMnemonic(mnemonic.mnemonicCode,
                                       kmParams,
                                       bip39PasswordOpt,
                                       mnemonic.creationTime))

      case Right(xprv: DecryptedExtPrivKey) =>
        val km = new BIP39KeyManager(xprv.xprv, kmParams, xprv.creationTime)
        Right(km)

      case Left(result) =>
        result match {
          case DecryptionError =>
            logger.error(s"Bad password for unlocking wallet!")
            Left(KeyManagerUnlockError.BadPassword)
          case JsonParsingError(message) =>
            logger.error(s"JSON parsing error when unlocking wallet: $message")
            Left(KeyManagerUnlockError.JsonParsingError(message))
          case ReadMnemonicError.NotFoundError =>
            logger.error(
              s"Encrypted mnemonic not found when unlocking the wallet!")
            Left(KeyManagerUnlockError.MnemonicNotFound)
        }
    }

  }
}
