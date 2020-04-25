package org.bitcoins.chain.models

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.gcs.FilterHeader

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
