package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.{AesEncryptedData, AesSalt}
import org.bitcoins.wallet.EncryptedMnemonic
import scodec.bits.ByteVector
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape

// todo: implement this as a flat file, not a table
class MnemonicCodeTable(tag: Tag)
    extends Table[EncryptedMnemonic](tag, "mnemonic_codes") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  //                            cipher      iv
  private type MnemonicTuple = (ByteVector, ByteVector, AesSalt)
  private val toTuple: EncryptedMnemonic => Option[MnemonicTuple] = code => {
    val EncryptedMnemonic(AesEncryptedData(cipherText, iv, salt)) = code
    Some((cipherText, iv, salt))
  }
  private val fromTuple: MnemonicTuple => EncryptedMnemonic = {
    case (cipherText, iv, salt) =>
      EncryptedMnemonic(AesEncryptedData(cipherText, iv, salt))
  }

  def cipherText = column[ByteVector]("ciphertext")
  def iv = column[ByteVector]("iv")
  def salt = column[AesSalt]("salt")

  override def * : ProvenShape[EncryptedMnemonic] =
    (cipherText, iv, salt) <> (fromTuple, toTuple)
}
