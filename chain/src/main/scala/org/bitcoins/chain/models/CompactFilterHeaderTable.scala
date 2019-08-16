package org.bitcoins.chain.models

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.gcs.FilterHeader
import slick.lifted.Tag
import slick.jdbc.SQLiteProfile.api._

case class CompactFilterHeaderDb(
  hashBE: DoubleSha256DigestBE,
  filterHashBE: DoubleSha256DigestBE,
  previousFilterHeaderBE: DoubleSha256DigestBE) {

  def filterHeader: FilterHeader = FilterHeader(hashBE.flip, previousFilterHeaderBE.flip)
}

object CompactFilterHeaderDbHelper {

  def fromFilterHeader(filterHeader: FilterHeader) =
    CompactFilterHeaderDb(
      hashBE = filterHeader.hash.flip,
      filterHashBE = filterHeader.filterHash.flip,
      previousFilterHeaderBE = filterHeader.prevHeaderHash.flip
    )
}

class CompactFilterHeaderTable(tag: Tag)
    extends Table[CompactFilterHeaderDb](tag, "cfheaders") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  def hash = column[DoubleSha256DigestBE]("hash", O.PrimaryKey)

  def filterHash = column[DoubleSha256DigestBE]("filter_hash")

  def previousFilterHeader = column[DoubleSha256DigestBE]("previous_filter_header")


  override def * = {
    (hash, filterHash, previousFilterHeader) <> (CompactFilterHeaderDb.tupled, CompactFilterHeaderDb.unapply)
  }
}
