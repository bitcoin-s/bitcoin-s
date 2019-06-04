package org.bitcoins.wallet

import java.nio.charset.StandardCharsets

import org.bitcoins.core.crypto.{
  AesCrypt,
  AesEncryptedData,
  AesPassword,
  MnemonicCode
}
import scodec.bits.ByteVector

import scala.util.Try

case class EncryptedMnemonic(value: AesEncryptedData) {
  import org.bitcoins.core.util.EitherUtil.EitherOps._

  def toMnemonic(passphrase: AesPassword): Try[MnemonicCode] = {
    AesCrypt.decrypt(value, passphrase).toTry.map { decrypted =>
      val wordsStr = new String(decrypted.toArray, StandardCharsets.UTF_8)
      val wordsVec = wordsStr.split(" ").toVector
      MnemonicCode.fromWords(wordsVec)
    }
  }
}

object EncryptedMnemonicHelper {
  import org.bitcoins.core.util.EitherUtil.EitherOps._

  def encrypt(
      mnemonicCode: MnemonicCode,
      passphrase: AesPassword): Try[EncryptedMnemonic] = {
    val wordsStr = mnemonicCode.words.mkString(" ")
    val Right(clearText) = ByteVector.encodeUtf8(wordsStr)

    AesCrypt
      .encrypt(clearText, passphrase)
      .toTry
      .map(EncryptedMnemonic(_))
  }
}
