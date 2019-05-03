package org.bitcoins.wallet

import ujson._

import scala.collection.JavaConverters._
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.crypto.AesPassword
import java.nio.file.Files
import org.bitcoins.core.crypto.MnemonicCode
import scodec.bits.ByteVector
import org.bitcoins.core.crypto.AesEncryptedData
import org.bitcoins.core.crypto.AesSalt
import scala.util.Failure
import scala.util.Success
import java.nio.file.Paths
import java.nio.file.Path
import scala.util.Try

// what do we do if seed exists? error if they aren't equal?
object WalletStorage extends BitcoinSLogger {

  private[wallet] val ENCRYPTED_SEED_FILE_NAME: String =
    "encrypted_bitcoin-s_seed.json"

  private object MnemonicJsonKeys {
    val IV = "iv"
    val CIPHER_TEXT = "cipherText"
    val SALT = "salt"
  }

  /**
    * Writes the encrypted mnemonic to disk
    */
  def writeMnemonicToDisk(
      mnemonic: EncryptedMnemonic,
      walletConfig: WalletAppConfig,
      isTest: Boolean = false): Path = {
    import mnemonic.{value => encrypted}

    val jsObject = {
      import MnemonicJsonKeys._
      Obj(
        IV -> encrypted.iv.toHex,
        CIPHER_TEXT -> encrypted.cipherText.toHex,
        SALT -> encrypted.salt.value.toHex,
      )
    }
    val path = {
      val dir = if (isTest) walletConfig.testDatadir else walletConfig.datadir
      dir.resolve(ENCRYPTED_SEED_FILE_NAME)
    }

    val writtenJs = write(jsObject)
    // todo, how to handle already existing seed?
    // if the found seed and the seed to write are
    // identical, we are good to go
    // if they are different we have a problem...
    if (Files.exists(path)) {
      val bakPath = Paths.get(path.toString + ".bak")
      Files.move(path, bakPath)
      logger.warn(s"Moved $path to $bakPath")
    }
    val writtenPath = Files.write(path, writtenJs.getBytes)
    logger.trace(s"Wrote encrypted mnemonic to $path")

    writtenPath
  }

  /**
    * Reads the wallet mmemonic from disk and tries to parse and
    * decrypt it
    */
  def readMnemonicFromDisk(
      passphrase: AesPassword,
      walletConfig: WalletAppConfig,
      isTest: Boolean = false): ReadMnemonicResult = {
    val path = {
      val dir = if (isTest) walletConfig.testDatadir else walletConfig.datadir
      dir.resolve(ENCRYPTED_SEED_FILE_NAME)
    }

    val rawJson = Files.readAllLines(path).asScala.mkString("\n")
    logger.debug(s"Read raw encrypted mnemonic from $path")

    val jsonE: Either[ReadMnemonicError, Value] = Try {
      read(rawJson)
    } match {
      case Failure(ParseException(clue, _, _, _)) =>
        Left(ReadMnemonicError.JsonParsingError(clue))
      case Failure(exception) => throw exception

      case Success(value) =>
        logger.debug(s"Parsed $path into valid json")
        Right(value)
    }

    import org.bitcoins.core.util.EitherUtil.EitherOps
    import MnemonicJsonKeys._
    import ReadMnemonicError._

    val readJsonTupleEither: Either[
      ReadMnemonicError,
      (String, String, String)] = jsonE.flatMap { json =>
      Try {
        val ivString = json(IV).str
        val cipherTextString = json(CIPHER_TEXT).str
        val rawSaltString = json(SALT).str
        (ivString, cipherTextString, rawSaltString)
      } match {
        case Success(value) => Right(value)
        case Failure(value: Value.InvalidData) =>
          logger.error(s"Error when parsing JSON file $path: ${value.msg}")
          Left(JsonParsingError(value.msg))
        case Failure(exception) => throw exception
      }
    }

    val encryptedEither: Either[ReadMnemonicError, EncryptedMnemonic] =
      readJsonTupleEither.flatMap {
        case (rawIv, rawCipherText, rawSalt) =>
          val encryptedOpt = for {
            iv <- ByteVector.fromHex(rawIv)
            cipherText <- ByteVector.fromHex(rawCipherText)
            rawSalt <- ByteVector.fromHex(rawSalt)
            salt = AesSalt(rawSalt)
          } yield {
            logger.debug(s"Parsed contents of $path into an EncryptedMnemonic")
            EncryptedMnemonic(AesEncryptedData(cipherText, iv, salt))
          }
          encryptedOpt
            .map(Right(_))
            .getOrElse(
              Left(JsonParsingError("JSON contents was not hex strings")))
      }

    val decryptedEither: Either[ReadMnemonicError, MnemonicCode] =
      encryptedEither.flatMap { encrypted =>
        encrypted.toMnemonic(passphrase) match {
          case Failure(exc) =>
            logger.error(s"Error when decrypting $encrypted: $exc")
            Left(DecryptionError)
          case Success(value) =>
            logger.debug(s"Decrypted $encrypted successfully")
            Right(value)
        }
      }

    decryptedEither match {
      case Left(value)  => value
      case Right(value) => ReadMnemonicSuccess(value)
    }
  }
}

/**
  * Represents the result of reading
  * an encrypted mnemonic from disk
  */
sealed trait ReadMnemonicResult

/** Represents the success case */
case class ReadMnemonicSuccess(mnemonic: MnemonicCode)
    extends ReadMnemonicResult

sealed trait ReadMnemonicError extends ReadMnemonicResult

object ReadMnemonicError {

  /**
    * Something went wrong while decrypting the mnemonic.
    * Most likely the passphrase was bad
    */
  case object DecryptionError extends ReadMnemonicError

  /**
    * Something went wrong while parsing the encrypted
    * mnemonic into valid JSON
    */
  case class JsonParsingError(message: String) extends ReadMnemonicError
}
