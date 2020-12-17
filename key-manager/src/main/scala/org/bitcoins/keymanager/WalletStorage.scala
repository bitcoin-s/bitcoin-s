package org.bitcoins.keymanager

import org.bitcoins.core.compat._
import org.bitcoins.core.crypto._
import org.bitcoins.crypto._
import scodec.bits.ByteVector
import ujson._

import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.NoSuchElementException
import scala.util.{Failure, Success, Try}

object WalletStorage extends KeyManagerLogger {

  val SEED_FOLDER_NAME: String = "seeds"

  val ENCRYPTED_SEED_FILE_NAME: String =
    "encrypted-bitcoin-s-seed.json"

  import org.bitcoins.core.compat.JavaConverters._

  /** Epoch time of the mainnet Genesis Block */
  val GENESIS_TIME = 1231006505L

  /** Start of bitcoin-s wallet project, Block 555,990 block time on 2018-12-28 */
  val FIRST_BITCOIN_S_WALLET_TIME = 1546042867L

  /** Checks if a wallet seed exists in datadir */
  def seedExists(seedPath: java.nio.file.Path): Boolean = {
    Files.exists(seedPath)
  }

  private object MnemonicJsonKeys {
    val IV = "iv"
    val CIPHER_TEXT = "cipherText"
    val SALT = "salt"
    val CREATION_TIME = "creationTime"
    val MNEMONIC_SEED = "mnemonicSeed"
    val XPRV = "xprv"
  }

  /**
    * Writes the mnemonic to disk.
    * If we encounter a file in the place we're about
    * to write to, an error will be thrown.
    */
  def writeSeedToDisk(seedPath: Path, seed: SeedState): Path = {
    seed match {
      case decryptedMnemonic: DecryptedMnemonic =>
        writeSeedToDisk(seedPath, decryptedMnemonic)
      case decryptedExtPrivKey: DecryptedExtPrivKey =>
        writeSeedToDisk(seedPath, decryptedExtPrivKey)
      case encrypted: EncryptedSeed =>
        writeSeedToDisk(seedPath, encrypted)
    }
  }

  /**
    * Writes the encrypted mnemonic to disk.
    * If we encounter a file in the place we're about
    * to write to, an error will be thrown.
    */
  def writeSeedToDisk(seedPath: Path, seed: EncryptedSeed): Path = {
    val encrypted = seed.value
    val jsObject = {
      import MnemonicJsonKeys._
      ujson.Obj(
        IV -> encrypted.iv.hex,
        CIPHER_TEXT -> encrypted.cipherText.toHex,
        SALT -> seed.salt.bytes.toHex,
        CREATION_TIME -> Num(seed.creationTime.getEpochSecond.toDouble)
      )
    }

    writeSeedJsonToDisk(seedPath, jsObject)
  }

  /**
    * Writes the unencrypted mnemonic to disk.
    * If we encounter a file in the place we're about
    * to write to, an error will be thrown.
    */
  def writeSeedToDisk(seedPath: Path, mnemonic: DecryptedMnemonic): Path = {
    val mnemonicCode = mnemonic.mnemonicCode
    val jsObject = {
      import MnemonicJsonKeys._
      ujson.Obj(
        MNEMONIC_SEED -> mnemonicCode.toVector,
        CREATION_TIME -> ujson.Num(
          mnemonic.creationTime.getEpochSecond.toDouble)
      )
    }

    writeSeedJsonToDisk(seedPath, jsObject)
  }

  /**
    * Writes the unencrypted xprv to disk.
    * If we encounter a file in the place we're about
    * to write to, an error will be thrown.
    */
  def writeSeedToDisk(seedPath: Path, extKey: DecryptedExtPrivKey): Path = {
    val jsObject = {
      import MnemonicJsonKeys._
      ujson.Obj(
        XPRV -> Str(extKey.xprv.toStringSensitive),
        CREATION_TIME -> ujson.Num(extKey.creationTime.getEpochSecond.toDouble)
      )
    }

    writeSeedJsonToDisk(seedPath, jsObject)
  }

  /**
    * Writes a seed's json output to disk.
    * If we encounter a file in the place we're about
    * to write to, an error will be thrown.
    */
  private def writeSeedJsonToDisk(seedPath: Path, jsObject: Obj): Path = {
    logger.info(s"Writing seed to $seedPath")

    val writtenJs = ujson.write(jsObject)

    def writeJsToDisk(): Path = {
      Files.createDirectories(seedPath.getParent)
      val writtenPath = Files.write(seedPath, writtenJs.getBytes())
      logger.trace(s"Wrote encrypted seed to $seedPath")

      writtenPath
    }

    // Check to see if a mnemonic exists already
    val hasMnemonic: Boolean = Files.isRegularFile(seedPath)

    if (hasMnemonic) {
      logger.info(s"$seedPath already exists")
      throw new RuntimeException(
        s"Attempting to overwrite an existing seed, this is dangerous! path: $seedPath")
    } else {
      logger.trace(s"$seedPath does not exist")
      writeJsToDisk()
    }
  }

  private def parseCreationTime(json: Value): Long = {
    Try(json(MnemonicJsonKeys.CREATION_TIME).num.toLong) match {
      case Success(value) =>
        value
      case Failure(_: NoSuchElementException) =>
        // If no CREATION_TIME is set, we set date to start of bitcoin-s wallet project
        // default is Block 555,990 block time on 2018-12-28
        FIRST_BITCOIN_S_WALLET_TIME
      case Failure(exception) => throw exception
    }
  }

  private def readJsonFromDisk(
      seedPath: Path): CompatEither[ReadMnemonicError, Value] = {
    if (Files.isRegularFile(seedPath)) {
      val rawJson = Files.readAllLines(seedPath).asScala.mkString("\n")
      logger.debug(s"Read raw mnemonic from $seedPath")

      Try(ujson.read(rawJson)) match {
        case Failure(ujson.ParseException(clue, _, _, _)) =>
          CompatLeft(ReadMnemonicError.JsonParsingError(clue))
        case Failure(exception) => throw exception
        case Success(value) =>
          logger.debug(s"Parsed $seedPath into valid json")
          CompatRight(value)
      }
    } else {
      logger.error(s"Mnemonic not found at $seedPath")
      CompatLeft(ReadMnemonicError.NotFoundError)
    }
  }

  /** Reads the raw encrypted mnemonic from disk,
    * performing no decryption
    */
  private def readEncryptedMnemonicFromDisk(
      seedPath: Path): CompatEither[ReadMnemonicError, EncryptedSeed] = {

    val jsonE = readJsonFromDisk(seedPath)

    import MnemonicJsonKeys._
    import ReadMnemonicError._

    val readJsonTupleEither: CompatEither[
      ReadMnemonicError,
      (String, String, String, Long)] = jsonE.flatMap { json =>
      logger.trace(s"Read encrypted mnemonic JSON: $json")
      Try {
        val creationTimeNum = parseCreationTime(json)
        val ivString = json(IV).str
        val cipherTextString = json(CIPHER_TEXT).str
        val rawSaltString = json(SALT).str
        (ivString, cipherTextString, rawSaltString, creationTimeNum)
      } match {
        case Success(value) => CompatRight(value)
        case Failure(exception) =>
          CompatLeft(JsonParsingError(exception.getMessage))
      }
    }

    val encryptedEither: CompatEither[ReadMnemonicError, EncryptedSeed] =
      readJsonTupleEither.flatMap {
        case (rawIv, rawCipherText, rawSalt, rawCreationTime) =>
          val encryptedOpt = for {
            iv <- ByteVector.fromHex(rawIv).map(AesIV.fromValidBytes)
            cipherText <- ByteVector.fromHex(rawCipherText)
            salt <- ByteVector.fromHex(rawSalt).map(AesSalt(_))
          } yield {
            logger.debug(
              s"Parsed contents of $seedPath into an EncryptedMnemonic")
            EncryptedSeed(AesEncryptedData(cipherText, iv),
                          salt,
                          Instant.ofEpochSecond(rawCreationTime))
          }
          val toRight: Option[CompatRight[ReadMnemonicError, EncryptedSeed]] =
            encryptedOpt
              .map(CompatRight(_))

          toRight.getOrElse(
            CompatLeft(JsonParsingError("JSON contents was not hex strings")))
      }
    encryptedEither
  }

  /** Reads the raw unencrypted mnemonic from disk */
  private def readUnencryptedMnemonicFromDisk(
      seedPath: Path): CompatEither[ReadMnemonicError, DecryptedMnemonic] = {

    val jsonE = readJsonFromDisk(seedPath)

    import MnemonicJsonKeys._
    import ReadMnemonicError._

    val readJsonTupleEither: CompatEither[
      ReadMnemonicError,
      (Vector[String], Long)] = jsonE.flatMap { json =>
      logger.trace(s"Read mnemonic JSON: Masked(json)")
      Try {
        val creationTimeNum = parseCreationTime(json)
        val words = json(MNEMONIC_SEED).arr.toVector.map(_.str)
        (words, creationTimeNum)
      } match {
        case Success(value) => CompatRight(value)
        case Failure(exception) =>
          CompatLeft(JsonParsingError(exception.getMessage))
      }
    }

    readJsonTupleEither.flatMap {
      case (words, rawCreationTime) =>
        val decryptedMnemonicT = for {
          mnemonicCodeT <- Try(MnemonicCode.fromWords(words))
        } yield {
          logger.debug(s"Parsed contents of $seedPath into a DecryptedMnemonic")
          DecryptedMnemonic(mnemonicCodeT,
                            Instant.ofEpochSecond(rawCreationTime))
        }

        val toRight: Try[CompatRight[ReadMnemonicError, DecryptedMnemonic]] =
          decryptedMnemonicT
            .map(CompatRight(_))

        toRight.getOrElse(
          CompatLeft(JsonParsingError("JSON contents was correctly formatted")))
    }
  }

  /** Reads the raw unencrypted xprv from disk */
  private def readUnencryptedSeedFromDisk(
      seedPath: Path): CompatEither[ReadMnemonicError, DecryptedExtPrivKey] = {

    val jsonE = readJsonFromDisk(seedPath)

    import MnemonicJsonKeys._
    import ReadMnemonicError._

    val readJsonTupleEither: CompatEither[ReadMnemonicError, (String, Long)] =
      jsonE.flatMap { json =>
        logger.trace(s"Read mnemonic JSON: Masked(json)")
        Try {
          val creationTimeNum = parseCreationTime(json)
          val xprvStr = json(XPRV).str
          (xprvStr, creationTimeNum)
        } match {
          case Success(value) => CompatRight(value)
          case Failure(exception) =>
            CompatLeft(JsonParsingError(exception.getMessage))
        }
      }

    readJsonTupleEither.flatMap {
      case (str, rawCreationTime) =>
        val decryptedExtPrivKeyT = ExtPrivateKey.fromStringT(str).map { xprv =>
          logger.debug(s"Parsed contents of $seedPath into a DecryptedMnemonic")
          DecryptedExtPrivKey(xprv, Instant.ofEpochSecond(rawCreationTime))
        }

        val toRight: Try[CompatRight[ReadMnemonicError, DecryptedExtPrivKey]] =
          decryptedExtPrivKeyT
            .map(CompatRight(_))

        toRight.getOrElse(
          CompatLeft(JsonParsingError("JSON contents was correctly formatted")))
    }
  }

  /**
    * Reads the wallet mnemonic from disk and tries to parse and
    * decrypt it
    */
  def decryptSeedFromDisk(
      seedPath: Path,
      passphraseOpt: Option[AesPassword]): Either[
    ReadMnemonicError,
    DecryptedSeedState] = {
    val decryptedEither: CompatEither[ReadMnemonicError, DecryptedSeedState] =
      passphraseOpt match {
        case Some(passphrase) =>
          val encryptedEither = readEncryptedMnemonicFromDisk(seedPath)

          encryptedEither.flatMap { encrypted =>
            encrypted.toMnemonic(passphrase) match {
              case Failure(_) =>
                encrypted.toExtPrivKey(passphrase) match {
                  case Failure(exc) =>
                    logger.error(s"Error when decrypting $encrypted: $exc")
                    CompatLeft(ReadMnemonicError.DecryptionError)
                  case Success(xprv) =>
                    logger.debug(s"Decrypted $encrypted successfully")
                    val decryptedExtPrivKey =
                      DecryptedExtPrivKey(xprv, encrypted.creationTime)
                    CompatRight(decryptedExtPrivKey)
                }
              case Success(mnemonic) =>
                logger.debug(s"Decrypted $encrypted successfully")
                val decryptedMnemonic =
                  DecryptedMnemonic(mnemonic, encrypted.creationTime)
                CompatRight(decryptedMnemonic)
            }
          }
        case None =>
          readUnencryptedMnemonicFromDisk(seedPath) match {
            case CompatLeft(_) =>
              readUnencryptedSeedFromDisk(seedPath)
            case CompatRight(mnemonic) => CompatRight(mnemonic)
          }
      }

    decryptedEither match {
      case CompatLeft(value)  => Left(value)
      case CompatRight(value) => Right(value)
    }
  }

  def changeAesPassword(
      seedPath: Path,
      oldPasswordOpt: Option[AesPassword],
      newPasswordOpt: Option[AesPassword]): SeedState = {
    logger.info("Changing encryption password for seed")
    decryptSeedFromDisk(seedPath, oldPasswordOpt) match {
      case Left(err) => sys.error(err.toString)
      case Right(decrypted) =>
        val fileName = seedPath.getFileName.toString
        logger.info("Creating backup file...")
        val backup = seedPath.getParent.resolve(fileName + ".backup")
        Files.move(seedPath, backup)

        val toWrite = newPasswordOpt match {
          case Some(pass) =>
            decrypted.encrypt(pass)
          case None =>
            decrypted
        }

        Try(writeSeedToDisk(seedPath, toWrite)) match {
          case Failure(exception) =>
            logger.error(
              s"Failed to write new seed, backup of previous seed file can be found at $backup")
            throw exception
          case Success(_) =>
            logger.info("Successfully wrote to disk, deleting backup file")
            Files.delete(backup)
            toWrite
        }
    }
  }

  def getPrivateKeyFromDisk(
      seedPath: Path,
      privKeyVersion: ExtKeyPrivVersion,
      passphraseOpt: Option[AesPassword],
      bip39PasswordOpt: Option[String]): ExtPrivateKeyHardened = {
    val decryptedSeed = decryptSeedFromDisk(seedPath, passphraseOpt) match {
      case Left(error)     => sys.error(error.toString)
      case Right(mnemonic) => mnemonic
    }

    decryptedSeed match {
      case DecryptedMnemonic(mnemonicCode, _) =>
        val seed = BIP39Seed.fromMnemonic(mnemonicCode, bip39PasswordOpt)
        seed.toExtPrivateKey(privKeyVersion).toHardened
      case DecryptedExtPrivKey(xprv, _) =>
        xprv.toHardened
    }
  }
}

sealed trait ReadMnemonicError { self: Error => }

object ReadMnemonicError {

  /**
    * Something went wrong while decrypting the mnemonic.
    * Most likely the passphrase was bad
    */
  case object DecryptionError
      extends Error(s"Could not decrypt mnemonic!")
      with ReadMnemonicError

  /**
    * Something went wrong while parsing the encrypted
    * mnemonic into valid JSON
    */
  case class JsonParsingError(message: String)
      extends Error(s"Error when parsing JSON: $message")
      with ReadMnemonicError

  /** The encrypted mnemonic was not found on disk */
  case object NotFoundError
      extends Error("Could not find mnemonic!")
      with ReadMnemonicError
}
