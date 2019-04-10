package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.MnemonicCode
import scodec.bits.ByteVector
import slick.jdbc.SQLiteProfile.api._

import scala.util.Try

case class EncryptedMnemonic(value: ByteVector) {

  // todo: use AES merged into master
  def toMnemonic(passphrase: String): Try[MnemonicCode] = ???
}

object EncryptedMnemonicHelper {

  // todo: use AES merged into master
  def encrypt(
      mnemonicCode: MnemonicCode,
      passphrase: String): EncryptedMnemonic = {
    val Right(cipherText) = ByteVector.encodeAscii("I am encrypted")
    EncryptedMnemonic(cipherText)
  }
}

class MnemonicCodeTable(tag: Tag)
    extends Table[EncryptedMnemonic](tag, "mnemonic_codes") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def bytes = column[ByteVector]("bytes")

  override def * =
    bytes.<>((bytes: ByteVector) => EncryptedMnemonic(bytes), {
      mnemonic: EncryptedMnemonic =>
        Some(mnemonic.value)
    })
}
