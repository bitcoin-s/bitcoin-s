package org.bitcoins.chain.models

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.gcs.FilterHeader
import slick.lifted.Tag
import slick.jdbc.SQLiteProfile.api._

case class CompactFilterHeaderDb(
    hashBE: DoubleSha256DigestBE,
    filterHashBE: DoubleSha256DigestBE,
    previousFilterHeaderBE: DoubleSha256DigestBE,
    blockHashBE: DoubleSha256DigestBE,
    height: Int) {

  def filterHeader: FilterHeader =
    FilterHeader(filterHashBE.flip, previousFilterHeaderBE.flip)

  override def toString: String = {
    s"CompactFilterDb(hashBE=$hashBE,filterHashBE=$filterHashBE,previousFilterHeaderBE=$previousFilterHeaderBE,blockHashBE=$blockHashBE,height=$height)"
  }
}

object CompactFilterHeaderDbHelper {

  def fromFilterHeader(
      filterHeader: FilterHeader,
      blockHash: DoubleSha256DigestBE,
      height: Int): CompactFilterHeaderDb =
    CompactFilterHeaderDb(
      hashBE = filterHeader.hash.flip,
      filterHashBE = filterHeader.filterHash.flip,
      previousFilterHeaderBE = filterHeader.prevHeaderHash.flip,
      blockHashBE = blockHash,
      height = height
    )
}

class CompactFilterHeaderTable(tag: Tag)
    extends Table[CompactFilterHeaderDb](tag, "cfheaders") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  def hash = column[DoubleSha256DigestBE]("hash", O.PrimaryKey)

  def filterHash = column[DoubleSha256DigestBE]("filter_hash")

  def previousFilterHeader =
    column[DoubleSha256DigestBE]("previous_filter_header")

  def blockHash = column[DoubleSha256DigestBE]("block_hash")

  def height = column[Int]("height")

  def heightIndex = index("cfheaders_height_index", height)

  def blockHashIndex = index("cfheaders_block_hash_index", blockHash)

  override def * = {
    (hash, filterHash, previousFilterHeader, blockHash, height) <> (CompactFilterHeaderDb.tupled, CompactFilterHeaderDb.unapply)
  }
}
