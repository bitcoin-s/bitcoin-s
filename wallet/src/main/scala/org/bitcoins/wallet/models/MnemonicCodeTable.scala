package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.MnemonicCode
import scodec.bits.ByteVector
import slick.jdbc.SQLiteProfile.api._

import scala.util.Try

case class EncryptedMnemonic(value: ByteVector) {
  // todo
  def toMnemonic(passphrase: String): Try[MnemonicCode] = ???
}

object EncryptedMnemonicHelper {

  // todo actually do this properly
  def encrypt(
      mnemonicCode: MnemonicCode,
      passphrase: String): EncryptedMnemonic =
    EncryptedMnemonic(
      ByteVector.encodeUtf8(mnemonicCode.words.mkString(", ")).right.get)
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
