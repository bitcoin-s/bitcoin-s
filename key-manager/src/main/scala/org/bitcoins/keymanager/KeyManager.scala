package org.bitcoins.keymanager

import java.nio.file.{Files, Path}

import org.bitcoins.core.compat.{CompatEither, CompatLeft, CompatRight}
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto._
import org.bitcoins.core.hd.{HDPath, HDPurpose}
import org.bitcoins.core.util.BitcoinSLogger
import scodec.bits.BitVector

import scala.util.{Failure, Success, Try}

case class KeyManager(private val mnemonic: MnemonicCode) {

  /** The wallet seed */
  private lazy val seed: BIP39Seed = BIP39Seed.fromMnemonic(mnemonic)

  /** Derives the relevant xpriv for the given HD purpose */
  def xprivForPurpose(
      purpose: HDPurpose,
      network: NetworkParameters): ExtPrivateKey = {
    val seed = BIP39Seed.fromMnemonic(mnemonic, BIP39Seed.EMPTY_PASSWORD) // todo think more about this

    val privVersion = HDUtil.getXprivVersion(purpose, network)
    seed.toExtPrivateKey(privVersion)
  }

  /** Converts a non-sensitive DB representation of a UTXO into
    * a signable (and sensitive) real-world UTXO
    */
  def toSign(
      privKeyPath: HDPath,
      purpose: HDPurpose,
      network: NetworkParameters): Sign = {
    val xpriv =
      xprivForPurpose(purpose, network).deriveChildPrivKey(privKeyPath)
    val privKey = xpriv.key
    val pubAtPath = privKey.publicKey

    val sign: Sign = Sign(privKey.signFunction, pubAtPath)

    sign
  }
}

object KeyManager extends CreateKeyManagerApi with BitcoinSLogger {

  def fromMnemonic(mnemonicCode: MnemonicCode): KeyManager = {
    new KeyManager(mnemonicCode)
  }

  /** Initializes the mnemonic seed and saves it to file */
  override def initializeWithEntropy(
      entropy: BitVector,
      seedPath: Path): InitializeKeyManagerResult = {

    logger.info(s"Initializing wallet with seedPath=${seedPath}")

    if (Files.notExists(seedPath)) {
      logger.info(
        s"Seed path parent directory does not exist, creating ${seedPath.getParent}")
      Files.createDirectories(seedPath.getParent)
    }

    val badPassphrase = AesPassword.fromString("bad-password").get
    val mnemonicT = Try(MnemonicCode.fromEntropy(entropy))
    val mnemonicE: CompatEither[InitializeKeyManagerError, MnemonicCode] =
      mnemonicT match {
        case Success(mnemonic) =>
          logger.info(s"Created mnemonic from entropy")
          CompatEither(Right(mnemonic))
        case Failure(err) =>
          logger.error(s"Could not create mnemonic from entropy! $err")
          CompatEither(Left(InitializeKeyManagerError.BadEntropy))
      }

    val encryptedMnemonicE: CompatEither[
      InitializeKeyManagerError,
      EncryptedMnemonic] =
      mnemonicE.map { EncryptedMnemonicHelper.encrypt(_, badPassphrase) }

    val biasedFinalEither: CompatEither[InitializeKeyManagerError, KeyManager] =
      for {
        mnemonic <- mnemonicE
        encrypted <- encryptedMnemonicE
        _ = {
          val mnemonicPath =
            WalletStorage.writeMnemonicToDisk(seedPath, encrypted)
          logger.info(s"Saved encrypted wallet mnemonic to $mnemonicPath")
        }

      } yield KeyManager(mnemonic)

    biasedFinalEither match {
      case CompatRight(keyManager) =>
        logger.info(s"Successfully initialized wallet")
        InitializeKeyManagerSuccess(keyManager = keyManager)
      case CompatLeft(err) =>
        logger.error(s"Failed to initialize key manager with err=${err}")
        err
    }
  }
}
