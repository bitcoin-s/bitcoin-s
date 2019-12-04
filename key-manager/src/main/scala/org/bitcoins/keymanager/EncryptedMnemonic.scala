package org.bitcoins.keymanager

import org.bitcoins.core.compat.CompatEither
import org.bitcoins.core.crypto._
import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}

case class EncryptedMnemonic(value: AesEncryptedData, salt: AesSalt) {

  def toMnemonic(password: AesPassword): Try[MnemonicCode] = {
    val key = password.toKey(salt)
    val either = AesCrypt.decrypt(value, key)
    CompatEither(either).toTry.flatMap { decrypted =>
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
