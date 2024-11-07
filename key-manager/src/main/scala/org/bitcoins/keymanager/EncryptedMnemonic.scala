package org.bitcoins.keymanager

import org.bitcoins.core.crypto._
import org.bitcoins.crypto.{AesCrypt, AesEncryptedData, AesPassword, AesSalt}
import scodec.bits.ByteVector

import java.time.Instant
import scala.util.{Failure, Success, Try}

sealed trait SeedState {
  def creationTime: Instant
  def backupTimeOpt: Option[Instant]
  def imported: Boolean
  def withBackupTime(backupTime: Instant): SeedState
}

sealed trait DecryptedSeedState extends SeedState {
  protected def strToEncrypt: String

  def encrypt(password: AesPassword): EncryptedSeed = {
    val clearTextE = ByteVector.encodeUtf8(strToEncrypt)
    clearTextE match {
      case Left(err) => throw err
      case Right(clearText) =>
        val (key, salt) = password.toKey

        val encrypted = AesCrypt.encrypt(clearText, key)

        EncryptedSeed(encrypted, salt, creationTime, backupTimeOpt, imported)
    }
  }
}

case class DecryptedMnemonic(
    private[keymanager] val mnemonicCode: MnemonicCode,
    creationTime: Instant,
    backupTimeOpt: Option[Instant],
    imported: Boolean
) extends DecryptedSeedState {
  override protected val strToEncrypt: String = mnemonicCode.words.mkString(" ")

  override def withBackupTime(backupTime: Instant): SeedState = {
    if (backupTimeOpt.isEmpty) copy(backupTimeOpt = Some(backupTime)) else this
  }
}

case class DecryptedExtPrivKey(
    private[keymanager] val xprv: ExtPrivateKey,
    creationTime: Instant,
    backupTimeOpt: Option[Instant],
    imported: Boolean
) extends DecryptedSeedState {
  override protected val strToEncrypt: String = xprv.toStringSensitive

  override def withBackupTime(backupTime: Instant): SeedState = {
    if (backupTimeOpt.isEmpty) copy(backupTimeOpt = Some(backupTime)) else this
  }
}

case class EncryptedSeed(
    value: AesEncryptedData,
    salt: AesSalt,
    creationTime: Instant,
    backupTimeOpt: Option[Instant],
    imported: Boolean
) extends SeedState {

  private def decryptStr(password: AesPassword): Try[String] = {
    val key = password.toKey(salt)
    val either = AesCrypt.decrypt(value, key)
    either.toTry.flatMap { decrypted =>
      decrypted.decodeUtf8 match {
        case Left(_) =>
          // when failing to decode this to a UTF-8 string
          // we assume it's because of a bad password
          Failure(ReadMnemonicError.DecryptionError)
        case Right(str) => Success(str)
      }
    }
  }

  def toMnemonic(password: AesPassword): Try[MnemonicCode] = {
    decryptStr(password).map { wordsStr =>
      val wordsVec = wordsStr.split(" ").toVector
      MnemonicCode.fromWords(wordsVec)
    }
  }

  def toExtPrivKey(password: AesPassword): Try[ExtPrivateKey] = {
    decryptStr(password).map(ExtPrivateKey.fromString)
  }

  override def withBackupTime(backupTime: Instant): SeedState = {
    if (backupTimeOpt.isEmpty) copy(backupTimeOpt = Some(backupTime)) else this
  }
}
