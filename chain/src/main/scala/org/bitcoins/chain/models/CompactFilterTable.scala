package org.bitcoins.chain.models

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.gcs.{BlockFilter, GolombFilter}
import scodec.bits.ByteVector
import slick.lifted.Tag
import slick.jdbc.SQLiteProfile.api._

case class CompactFilterDb(
    hashBE: DoubleSha256DigestBE,
    filterType: Short,
    bytes: ByteVector,
    height: Int,
    blockHash: DoubleSha256DigestBE) {

  def golombFilter: GolombFilter = filterType match {
    case 0 => BlockFilter.fromBytes(bytes, blockHash.flip)
    case _ => throw new RuntimeException(s"Invalid filter type $filterType")
  }
}

object CompactFilterDbHelper {
  def fromGolombFilter(golombFilter: GolombFilter, blockHash: DoubleSha256DigestBE, height: Int): CompactFilterDb =
    CompactFilterDb(golombFilter.hash.flip, 0, golombFilter.bytes, height, blockHash)
}

class CompactFilterTable(tag: Tag)
  extends Table[CompactFilterDb](tag, "cfilters") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  def hash = column[DoubleSha256DigestBE]("hash", O.PrimaryKey)

  def filterType = column[Short]("filter_type")

  def bytes = column[ByteVector]("bytes")

  def height = column[Int]("height")

  def blockHash = column[DoubleSha256DigestBE]("block_hash")

  def heightIndex = index("cfilters_height_index", height)

  def blockHashIndex = index("cfilters_block_hash_index", blockHash)

  override def * = {
    (hash, filterType, bytes, height, blockHash) <> (CompactFilterDb.tupled, CompactFilterDb.unapply)
  }
}
