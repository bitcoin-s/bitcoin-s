package org.bitcoins.wallet

import scala.collection.JavaConverters._
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
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.core.crypto.AesIV

// what do we do if seed exists? error if they aren't equal?
object WalletStorage extends KeyHandlingLogger {

  /** Checks if a wallet seed exists in datadir */
  def seedExists()(implicit config: WalletAppConfig): Boolean = {
    val seedPath = config.datadir.resolve(ENCRYPTED_SEED_FILE_NAME)
    Files.exists(seedPath)
  }

  private[wallet] val ENCRYPTED_SEED_FILE_NAME: String =
    "encrypted_bitcoin-s_seed.json"

  private object MnemonicJsonKeys {
    val IV = "iv"
    val CIPHER_TEXT = "cipherText"
    val SALT = "salt"
  }

  /**
    * Writes the encrypted mnemonic to disk.
    * If we encounter a file in the place we're about
    * to write to, we move it to a backup location
    * with the current epoch timestamp as part of
    * the file name.
    */
  def writeMnemonicToDisk(mnemonic: EncryptedMnemonic)(
      implicit config: WalletAppConfig): Path = {
    import mnemonic.{value => encrypted}

    val jsObject = {
      import MnemonicJsonKeys._
      ujson.Obj(
        IV -> encrypted.iv.hex,
        CIPHER_TEXT -> encrypted.cipherText.toHex,
        SALT -> mnemonic.salt.bytes.toHex
      )
    }

    val path = config.datadir.resolve(ENCRYPTED_SEED_FILE_NAME)

    logger.debug(s"Writing mnemonic to $path")

    val writtenJs = ujson.write(jsObject)

    def writeJsToDisk() = {
      val writtenPath = Files.write(path, writtenJs.getBytes())
      logger.trace(s"Wrote encrypted mnemonic to $path")

      writtenPath
    }

    val foundMnemonicOpt: Option[EncryptedMnemonic] =
      readEncryptedMnemonicFromDisk() match {
        case Left(_) =>
          None
        case Right(mnemonic) => Some(mnemonic)
      }

    foundMnemonicOpt match {
      case None =>
        logger.trace(s"$path does not exist")
        writeJsToDisk()
      case Some(found) =>
        logger.trace(s"$path already exists")
        if (found == mnemonic) {
          logger.trace(s"Found and provided mnemonics are the same, skipping")
          path
        } else {
          logger.warn(
            s"Found mnemonic on disk is not the same as mnemonic we're about to write")

          val bakPath = {
            val epoch = System.currentTimeMillis.toString
            Paths.get(s"${path.toString()}-$epoch.bak")
          }

          logger.trace(s"Moving file to $bakPath")
          Files.move(path, bakPath)

          logger.warn(s"Moved $path to $bakPath")
          writeJsToDisk()
        }
    }
  }

  /** Reads the raw encrypted mnemonic from disk,
    * performing no decryption
    */
  private def readEncryptedMnemonicFromDisk()(
      implicit config: WalletAppConfig): Either[
    ReadMnemonicError,
    EncryptedMnemonic] = {

    val path = {
      config.datadir.resolve(ENCRYPTED_SEED_FILE_NAME)
    }

    val jsonE: Either[ReadMnemonicError, ujson.Value] = {
      if (Files.isRegularFile(path)) {
        val rawJson = Files.readAllLines(path).asScala.mkString("\n")
        logger.debug(s"Read raw encrypted mnemonic from $path")

        Try {
          ujson.read(rawJson)
        } match {
          case Failure(ujson.ParseException(clue, _, _, _)) =>
            Left(ReadMnemonicError.JsonParsingError(clue))
          case Failure(exception) => throw exception

          case Success(value) =>
            logger.debug(s"Parsed $path into valid json")
            Right(value)
        }
      } else {
        logger.error(s"Encrypted mnemonic not found at $path")
        Left(ReadMnemonicError.NotFoundError)
      }
    }

    import org.bitcoins.core.util.EitherUtil.EitherOps._
    import MnemonicJsonKeys._
    import ReadMnemonicError._

    val readJsonTupleEither: Either[
      ReadMnemonicError,
      (String, String, String)] = jsonE.flatMap { json =>
      logger.trace(s"Read encrypted mnemonic JSON: $json")
      Try {
        val ivString = json(IV).str
        val cipherTextString = json(CIPHER_TEXT).str
        val rawSaltString = json(SALT).str
        (ivString, cipherTextString, rawSaltString)
      } match {
        case Success(value) => Right(value)
        case Failure(value: ujson.Value.InvalidData) =>
          logger.error(s"Error when parsing JSON file $path: ${value.msg}")
          Left(JsonParsingError(value.msg))
        case Failure(exception) => throw exception
      }
    }

    val encryptedEither: Either[ReadMnemonicError, EncryptedMnemonic] =
      readJsonTupleEither.flatMap {
        case (rawIv, rawCipherText, rawSalt) =>
          val encryptedOpt = for {
            iv <- ByteVector.fromHex(rawIv).map(AesIV.fromValidBytes(_))
            cipherText <- ByteVector.fromHex(rawCipherText)
            salt <- ByteVector.fromHex(rawSalt).map(AesSalt(_))
          } yield {
            logger.debug(s"Parsed contents of $path into an EncryptedMnemonic")
            EncryptedMnemonic(AesEncryptedData(cipherText, iv), salt)
          }
          encryptedOpt
            .map(Right(_))
            .getOrElse(
              Left(JsonParsingError("JSON contents was not hex strings")))
      }
    encryptedEither
  }

  /**
    * Reads the wallet mmemonic from disk and tries to parse and
    * decrypt it
    */
  def decryptMnemonicFromDisk(passphrase: AesPassword)(
      implicit
      config: WalletAppConfig): ReadMnemonicResult = {

    val encryptedEither = readEncryptedMnemonicFromDisk()

    import org.bitcoins.core.util.EitherUtil.EitherOps._
    val decryptedEither: Either[ReadMnemonicError, MnemonicCode] =
      encryptedEither.flatMap { encrypted =>
        encrypted.toMnemonic(passphrase) match {
          case Failure(exc) =>
            logger.error(s"Error when decrypting $encrypted: $exc")
            Left(ReadMnemonicError.DecryptionError)
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

sealed trait ReadMnemonicError extends ReadMnemonicResult { self: Error =>
}

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
