package org.bitcoins.wallet.models

import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.wallet.utxo.AddressTag
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{PrimaryKey, ProvenShape}

abstract class AddressTagDb[AddressTagType <: AddressTag] {
  def address: BitcoinAddress

  def tag: AddressTagType
}

abstract class AddressTagTable[
    AddressTagType <: AddressTag,
    AddressTagDbType <: AddressTagDb[AddressTagType]](t: Tag, tableName: String)
    extends Table[AddressTagDbType](t, tableName) {

  import org.bitcoins.db.DbCommonsColumnMappers._

  implicit val mapper: BaseColumnType[AddressTagType]

  def address: Rep[BitcoinAddress] = column[BitcoinAddress]("address", O.Unique)

  def tag: Rep[AddressTagType] = column[AddressTagType]("tag")(mapper)

  type AddressTagTuple = (BitcoinAddress, AddressTagType)

  val fromTuple: AddressTagTuple => AddressTagDbType

  val toTuple: AddressTagDbType => Option[AddressTagTuple] =
    AddressTag => Some((AddressTag.address, AddressTag.tag))

  def * : ProvenShape[AddressTagDbType]

  /** All Addresss must have a OutPoint in the wallet*/
  def fk_address = {
    val addressTable = TableQuery[AddressTable]
    foreignKey("fk_address",
               sourceColumns = address,
               targetTableQuery = addressTable)(_.address)
  }

  def primaryKey: PrimaryKey =
    primaryKey("pk_address", sourceColumns = address)

}
