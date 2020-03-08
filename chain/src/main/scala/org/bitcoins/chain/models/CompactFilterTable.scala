package org.bitcoins.chain.models

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.gcs.{BlockFilter, FilterType, GolombFilter}
import org.bitcoins.core.util.CryptoUtil
import scodec.bits.ByteVector
import slick.lifted.Tag
import slick.jdbc.SQLiteProfile.api._

case class CompactFilterDb(
    hashBE: DoubleSha256DigestBE,
    filterType: FilterType,
    bytes: ByteVector,
    height: Int,
    blockHashBE: DoubleSha256DigestBE) {
  require(
    CryptoUtil.doubleSHA256(bytes).flip == hashBE,
    s"Bytes must hash to hashBE! It looks like you didn't construct CompactFilterDb correctly")

  def golombFilter: GolombFilter = filterType match {
    case FilterType.Basic => BlockFilter.fromBytes(bytes, blockHashBE.flip)
  }

  override def toString: String = {
    s"CompactFilterDb(hashBE=$hashBE,filterType=$filterType,height=$height,blockHashBE=$blockHashBE,bytes=${bytes})"
  }
}

object CompactFilterDbHelper {

  def fromGolombFilter(
      golombFilter: GolombFilter,
      blockHash: DoubleSha256DigestBE,
      height: Int): CompactFilterDb =
    fromFilterBytes(golombFilter.bytes, blockHash, height)

  def fromFilterBytes(
      filterBytes: ByteVector,
      blockHash: DoubleSha256DigestBE,
      height: Int): CompactFilterDb =
    CompactFilterDb(CryptoUtil.doubleSHA256(filterBytes).flip,
                    FilterType.Basic,
                    filterBytes,
                    height,
                    blockHash)
}

class CompactFilterTable(tag: Tag)
    extends Table[CompactFilterDb](tag, "cfilters") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  def hash = column[DoubleSha256DigestBE]("hash")

  def filterType = column[FilterType]("filter_type")

  def bytes = column[ByteVector]("bytes")

  def height = column[Int]("height")

  def blockHash = column[DoubleSha256DigestBE]("block_hash", O.PrimaryKey)

  def heightIndex = index("cfilters_height_index", height)

  def hashIndex = index("cfilters_hash_index", hash)

  override def * = {
    (hash, filterType, bytes, height, blockHash) <> (CompactFilterDb.tupled, CompactFilterDb.unapply)
  }
}
