package org.bitcoins.keymanager.bip39

import java.nio.file.Files
import java.time.Instant

import org.bitcoins.core.compat.{CompatEither, CompatLeft, CompatRight}
import org.bitcoins.core.crypto._
import org.bitcoins.core.hd.{HDAccount, HDPath}
import org.bitcoins.core.util.{BitcoinSLogger, TimeUtil}
import org.bitcoins.crypto.{AesPassword, Sign}
import org.bitcoins.keymanager._
import org.bitcoins.keymanager.util.HDUtil
import scodec.bits.BitVector

import scala.util.{Failure, Success, Try}

/**
  * This is a key manager implementation meant to represent an in memory
  * BIP39 key manager
  *
  * @param mnemonic the mnemonic seed used for this wallet
  * @param kmParams the parameters used to generate the right keychain
  * @see https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki
  */
case class BIP39KeyManager(
    private val mnemonic: MnemonicCode,
    kmParams: KeyManagerParams,
    private val bip39PasswordOpt: Option[String],
    creationTime: Instant)
    extends KeyManager {
  private val seed = bip39PasswordOpt match {
    case Some(pw) =>
      BIP39Seed.fromMnemonic(mnemonic = mnemonic, password = pw)
    case None =>
      BIP39Seed.fromMnemonic(mnemonic = mnemonic,
                             password = BIP39Seed.EMPTY_PASSWORD)
  }

  def ==(km: KeyManager): Boolean =
    km match {
      case bip39Km: BIP39KeyManager =>
        mnemonic == bip39Km.mnemonic &&
          kmParams == bip39Km.kmParams &&
          bip39PasswordOpt == bip39Km.bip39PasswordOpt &&
          creationTime.getEpochSecond == bip39Km.creationTime.getEpochSecond
      case _: KeyManager =>
        km == this
    }

  private val privVersion: ExtKeyPrivVersion =
    HDUtil.getXprivVersion(kmParams.purpose, kmParams.network)

  /** FIXME Temporary public, should be private, This is still required to make the
    * BinaryOutcomeDLCClient from the wallet, can change back to private val when we change the client to use a Sign instead */
  val rootExtPrivKey: ExtPrivateKey = seed.toExtPrivateKey(privVersion)

  /** Converts a non-sensitive DB representation of a UTXO into
    * a signable (and sensitive) real-world UTXO
    */
  def toSign(privKeyPath: HDPath): Sign = {
    val xpriv =
      rootExtPrivKey.deriveChildPrivKey(privKeyPath)

    xpriv
  }

  def deriveXPub(account: HDAccount): Try[ExtPublicKey] = {
    rootExtPrivKey.deriveChildPubKey(account)
  }

  /** Returns the root [[ExtPublicKey]] */
  def getRootXPub: ExtPublicKey = {
    rootExtPrivKey.extPublicKey
  }
}

object BIP39KeyManager extends BIP39KeyManagerCreateApi with BitcoinSLogger {
  val badPassphrase = AesPassword.fromString("changeMe").get

  /** Initializes the mnemonic seed and saves it to file */
  override def initializeWithEntropy(
      entropy: BitVector,
      bip39PasswordOpt: Option[String],
      kmParams: KeyManagerParams): Either[
    KeyManagerInitializeError,
    BIP39KeyManager] = {
    val seedPath = kmParams.seedPath
    logger.info(s"Initializing wallet with seedPath=${seedPath}")

    val time = TimeUtil.now

    val writtenToDiskE: CompatEither[KeyManagerInitializeError, KeyManager] =
      if (Files.notExists(seedPath)) {
        logger.info(
          s"Seed path parent directory does not exist, creating ${seedPath.getParent}")
        Files.createDirectories(seedPath.getParent)

        val mnemonicT = Try(MnemonicCode.fromEntropy(entropy))
        val mnemonicE: CompatEither[KeyManagerInitializeError, MnemonicCode] =
          mnemonicT match {
            case Success(mnemonic) =>
              logger.info(s"Created mnemonic from entropy")
              CompatEither(Right(mnemonic))
            case Failure(err) =>
              logger.error(s"Could not create mnemonic from entropy! $err")
              CompatEither(Left(InitializeKeyManagerError.BadEntropy))
          }

        val encryptedMnemonicE: CompatEither[
          KeyManagerInitializeError,
          EncryptedMnemonic] =
          mnemonicE.map { mnemonic =>
            EncryptedMnemonicHelper.encrypt(DecryptedMnemonic(mnemonic, time),
                                            badPassphrase)
          }

        for {
          mnemonic <- mnemonicE
          encrypted <- encryptedMnemonicE
          _ = {
            val mnemonicPath =
              WalletStorage.writeMnemonicToDisk(seedPath, encrypted)
            logger.info(s"Saved encrypted wallet mnemonic to $mnemonicPath")
          }

        } yield {
          BIP39KeyManager(mnemonic = mnemonic,
                          kmParams = kmParams,
                          bip39PasswordOpt = bip39PasswordOpt,
                          creationTime = time)
        }
      } else {
        logger.info(
          s"Seed file already exists, attempting to initialize form existing seed file=$seedPath.")

        WalletStorage.decryptMnemonicFromDisk(kmParams.seedPath, badPassphrase) match {
          case Right(mnemonic) =>
            CompatRight(
              BIP39KeyManager(mnemonic = mnemonic.mnemonicCode,
                              kmParams = kmParams,
                              bip39PasswordOpt = bip39PasswordOpt,
                              creationTime = mnemonic.creationTime))
          case Left(err) =>
            CompatLeft(
              InitializeKeyManagerError.FailedToReadWrittenSeed(
                KeyManagerUnlockError.JsonParsingError(err.toString)))
        }
      }

    //verify we can unlock it for a sanity check
    val unlocked = BIP39LockedKeyManager.unlock(passphrase = badPassphrase,
                                                bip39PasswordOpt =
                                                  bip39PasswordOpt,
                                                kmParams = kmParams)

    val biasedFinalE: CompatEither[KeyManagerInitializeError, BIP39KeyManager] =
      for {
        kmBeforeWrite <- writtenToDiskE
        invariant <- unlocked match {
          case Right(unlockedKeyManager) =>
            require(
              unlockedKeyManager == kmBeforeWrite,
              s"We could not read the key manager we just wrote! $kmBeforeWrite != $unlockedKeyManager"
            )
            CompatRight(unlockedKeyManager)

          case Left(err) =>
            CompatLeft(InitializeKeyManagerError.FailedToReadWrittenSeed(err))
        }
      } yield {
        invariant
      }

    biasedFinalE match {
      case CompatRight(initSuccess) =>
        logger.info(s"Successfully initialized wallet")
        Right(initSuccess)
      case CompatLeft(err) =>
        logger.error(s"Failed to initialize key manager with err=${err}")
        Left(err)
    }
  }

  /** Reads the key manager from disk and decrypts it with the given password */
  def fromParams(
      kmParams: KeyManagerParams,
      password: AesPassword,
      bip39PasswordOpt: Option[String]): Either[
    ReadMnemonicError,
    BIP39KeyManager] = {
    val mnemonicCodeE =
      WalletStorage.decryptMnemonicFromDisk(kmParams.seedPath, password)

    mnemonicCodeE match {
      case Right(mnemonic) =>
        Right(
          new BIP39KeyManager(mnemonic.mnemonicCode,
                              kmParams,
                              bip39PasswordOpt,
                              mnemonic.creationTime))
      case Left(v) => Left(v)
    }
  }
}
