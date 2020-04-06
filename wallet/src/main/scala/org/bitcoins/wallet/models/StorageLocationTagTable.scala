package org.bitcoins.wallet.models

import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.wallet.utxo.StorageLocationTag
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape

case class StorageLocationTagDb(
    address: BitcoinAddress,
    tag: StorageLocationTag)
    extends AddressTagDb[StorageLocationTag] {}

class StorageLocationTagTable(t: Tag)
    extends AddressTagTable[StorageLocationTag, StorageLocationTagDb](
      t,
      tableName = "storage_location_tags") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  implicit override val mapper: BaseColumnType[StorageLocationTag] =
    MappedColumnType
      .base[StorageLocationTag, String](_.toString,
                                        StorageLocationTag.fromString(_).get)

  override val fromTuple: (
      (BitcoinAddress, StorageLocationTag)) => StorageLocationTagDb = {
    case (address, tag) => StorageLocationTagDb(address, tag)
  }

  override def * : ProvenShape[StorageLocationTagDb] =
    (address, tag) <> (fromTuple, toTuple)
}
