package org.bitcoins.core.api.chain.db

import org.bitcoins.core.gcs.FilterHeader
import org.bitcoins.crypto.DoubleSha256DigestBE

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
