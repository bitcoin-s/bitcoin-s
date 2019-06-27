package org.bitcoins.wallet

import java.nio.charset.StandardCharsets

import org.bitcoins.core.crypto.{AesCrypt, AesEncryptedData, MnemonicCode}
import scodec.bits.ByteVector

import scala.util.Try
import org.bitcoins.core.crypto.AesSalt
import org.bitcoins.core.crypto.AesPassword
import org.bitcoins.core.util.BitcoinSLogger
import scala.util.Success
import scala.util.Failure

case class EncryptedMnemonic(value: AesEncryptedData, salt: AesSalt)
    extends BitcoinSLogger {
  import org.bitcoins.core.util.EitherUtil.EitherOps._

  def toMnemonic(password: AesPassword): Try[MnemonicCode] = {
    val key = password.toKey(salt)
    AesCrypt.decrypt(value, key).toTry.flatMap { decrypted =>
      decrypted.decodeUtf8 match {
        case Left(_) =>
          // when failing to decode this to a UTF-8 string
          // we assume it's because of a bad password
          Failure(ReadMnemonicError.DecryptionError)

        case Right(wordsStr) =>
          val wordsVec = wordsStr.split(" ").toVector
          Success(MnemonicCode.fromWords(wordsVec))
      }
    }
  }
}

object EncryptedMnemonicHelper {

  def encrypt(
      mnemonicCode: MnemonicCode,
      password: AesPassword): EncryptedMnemonic = {
    val wordsStr = mnemonicCode.words.mkString(" ")
    val Right(clearText) = ByteVector.encodeUtf8(wordsStr)

    val (key, salt) = password.toKey

    val encryted = AesCrypt
      .encrypt(clearText, key)

    EncryptedMnemonic(encryted, salt)
  }
}
