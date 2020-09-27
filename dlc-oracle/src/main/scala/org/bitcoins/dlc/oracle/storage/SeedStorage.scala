package org.bitcoins.dlc.oracle.storage

import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.NoSuchElementException

import org.bitcoins.core.crypto.ExtKeyVersion.SegWitMainNetPriv
import org.bitcoins.core.crypto.{BIP39Seed, ExtPrivateKey}
import org.bitcoins.crypto._
import org.bitcoins.keymanager.{DecryptedMnemonic, EncryptedMnemonic}
import org.slf4j.LoggerFactory
import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}

object SeedStorage {

  val ENCRYPTED_SEED_FILE_NAME: String = "encrypted-seed.json"

  import org.bitcoins.core.compat.JavaConverters._

  /** Start of dlc oracle project, Block 649 071 block time on 2020-09-19 */
  val FIRST_WALLET_TIME = 1600518676L

  private val logger = LoggerFactory.getLogger(getClass)

  /** Checks if a wallet seed exists in datadir */
  def seedExists(seedPath: Path): Boolean = {
    Files.exists(seedPath)
  }

  private object MnemonicJsonKeys {
    val IV = "iv"
    val CIPHER_TEXT = "cipherText"
    val SALT = "salt"
    val CREATION_TIME = "creationTime"
  }

  /**
    * Writes the encrypted mnemonic to disk.
    * If we encounter a file in the place we're about
    * to write to, we move it to a backup location
    * with the current epoch timestamp as part of
    * the file name.
    */
  def writeMnemonicToDisk(seedPath: Path, mnemonic: EncryptedMnemonic): Path = {
    val encrypted = mnemonic.value
    val jsObject = {
      import MnemonicJsonKeys._
      ujson.Obj(
        IV -> encrypted.iv.hex,
        CIPHER_TEXT -> encrypted.cipherText.toHex,
        SALT -> mnemonic.salt.bytes.toHex,
        CREATION_TIME -> ujson.Num(
          mnemonic.creationTime.getEpochSecond.toDouble)
      )
    }

    logger.info(s"Writing mnemonic to $seedPath")

    val writtenJs = ujson.write(jsObject)

    def writeJsToDisk(): Path = {
      val directory = seedPath.getParent

      if (!Files.isDirectory(directory)) {
        Files.createDirectory(directory)
      }

      val writtenPath = Files.write(seedPath, writtenJs.getBytes())
      logger.trace(s"Wrote encrypted mnemonic to $seedPath")

      writtenPath
    }

    //check to see if a mnemonic exists already...
    val foundMnemonicOpt: Option[EncryptedMnemonic] =
      readEncryptedMnemonicFromDisk(seedPath) match {
        case Left(_) =>
          None
        case Right(mnemonic) => Some(mnemonic)
      }

    foundMnemonicOpt match {
      case None =>
        logger.trace(s"$seedPath does not exist")
        writeJsToDisk()
      case Some(_) =>
        logger.info(s"$seedPath already exists")
        throw new RuntimeException(
          s"Attempting to overwrite an existing mnemonic seed, this is dangerous!")
    }
  }

  /** Reads the raw encrypted mnemonic from disk,
    * performing no decryption
    */
  private def readEncryptedMnemonicFromDisk(
      seedPath: Path): Either[ReadMnemonicError, EncryptedMnemonic] = {

    val jsonE: Either[ReadMnemonicError, ujson.Value] = {
      if (Files.isRegularFile(seedPath)) {
        val rawJson = Files.readAllLines(seedPath).asScala.mkString("\n")
        logger.debug(s"Read raw encrypted mnemonic from $seedPath")

        Try {
          ujson.read(rawJson)
        } match {
          case Failure(ujson.ParseException(clue, _, _, _)) =>
            Left(ReadMnemonicError.JsonParsingError(clue))
          case Failure(exception) => throw exception

          case Success(value) =>
            logger.debug(s"Parsed $seedPath into valid json")
            Right(value)
        }
      } else {
        logger.error(s"Encrypted mnemonic not found at $seedPath")
        Left(ReadMnemonicError.NotFoundError)
      }
    }

    import MnemonicJsonKeys._
    import ReadMnemonicError._

    val readJsonTupleEither: Either[
      ReadMnemonicError,
      (String, String, String, Long)] = jsonE.flatMap { json =>
      logger.trace(s"Read encrypted mnemonic JSON: $json")
      val creationTimeNum = Try(json(CREATION_TIME).num.toLong) match {
        case Success(value) =>
          value
        case Failure(err) if err.isInstanceOf[NoSuchElementException] =>
          FIRST_WALLET_TIME
        case Failure(exception) => throw exception
      }
      Try {
        val ivString = json(IV).str
        val cipherTextString = json(CIPHER_TEXT).str
        val rawSaltString = json(SALT).str
        (ivString, cipherTextString, rawSaltString, creationTimeNum)
      } match {
        case Success(value)     => Right(value)
        case Failure(exception) => throw exception
      }
    }

    val encryptedEither: Either[ReadMnemonicError, EncryptedMnemonic] =
      readJsonTupleEither.flatMap {
        case (rawIv, rawCipherText, rawSalt, rawCreationTime) =>
          val encryptedOpt = for {
            iv <- ByteVector.fromHex(rawIv).map(AesIV.fromValidBytes)
            cipherText <- ByteVector.fromHex(rawCipherText)
            salt <- ByteVector.fromHex(rawSalt).map(AesSalt(_))
          } yield {
            logger.debug(
              s"Parsed contents of $seedPath into an EncryptedMnemonic")
            EncryptedMnemonic(AesEncryptedData(cipherText, iv),
                              salt,
                              Instant.ofEpochSecond(rawCreationTime))
          }
          val toRight: Option[Right[ReadMnemonicError, EncryptedMnemonic]] =
            encryptedOpt
              .map(Right(_))

          toRight.getOrElse(
            Left(JsonParsingError("JSON contents was not hex strings")))
      }
    encryptedEither
  }

  /**
    * Reads the wallet mnemonic from disk and tries to parse and
    * decrypt it
    */
  def decryptMnemonicFromDisk(
      seedPath: Path,
      passphrase: AesPassword): DecryptedMnemonic = {

    val encryptedEither = readEncryptedMnemonicFromDisk(seedPath)

    val decryptedEither: Either[ReadMnemonicError, DecryptedMnemonic] =
      encryptedEither.flatMap { encrypted =>
        encrypted.toMnemonic(passphrase) match {
          case Failure(exc) =>
            logger.error(s"Error when decrypting $encrypted: $exc")
            Left(ReadMnemonicError.DecryptionError)
          case Success(mnemonic) =>
            logger.debug(s"Decrypted $encrypted successfully")
            val decryptedMnemonic =
              DecryptedMnemonic(mnemonic, encrypted.creationTime)
            Right(decryptedMnemonic)
        }
      }

    decryptedEither match {
      case Left(err)    => sys.error(err.toString)
      case Right(value) => value
    }
  }

  def getPrivateKeyFromDisk(
      seedPath: Path,
      passphrase: AesPassword,
      bip39PasswordOpt: Option[String]): ExtPrivateKey = {
    val mnemonic = decryptMnemonicFromDisk(seedPath, passphrase).mnemonicCode
    val seed = bip39PasswordOpt match {
      case Some(pw) =>
        BIP39Seed.fromMnemonic(mnemonic = mnemonic, password = pw)
      case None =>
        BIP39Seed.fromMnemonic(mnemonic = mnemonic,
                               password = BIP39Seed.EMPTY_PASSWORD)
    }

    seed.toExtPrivateKey(SegWitMainNetPriv)
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
