package org.bitcoins.core.gcs

import org.bitcoins.crypto.{
  CryptoUtil,
  DoubleSha256Digest,
  DoubleSha256DigestBE
}

/** Bip 157 Block Filter Headers which commit to a chain of block filters,
  * much in the same way that block headers commit to a block chain
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#filter-headers]]
  */
case class FilterHeader(
    filterHash: DoubleSha256Digest,
    prevHeaderHash: DoubleSha256Digest) {

  val hash: DoubleSha256Digest = {
    CryptoUtil.doubleSHA256(filterHash.bytes ++ prevHeaderHash.bytes)
  }

  /** Given the next Block Filter, constructs the next Block Filter Header */
  def nextHeader(nextFilter: GolombFilter): FilterHeader = {
    FilterHeader(filterHash = nextFilter.hash, prevHeaderHash = this.hash)
  }

  /** Given the next Block Filter hash, constructs the next Block Filter Header */
  def nextHeader(nextFilterHash: DoubleSha256Digest): FilterHeader = {
    FilterHeader(filterHash = nextFilterHash, prevHeaderHash = this.hash)
  }

  override def toString: String = {
    s"FilterHeader(hashBE=${hash.flip},filterHashBE=${filterHash.flip.hex},prevHeaderHashBE=${prevHeaderHash.flip.hex})"
  }
}

object FilterHeader {

  def apply(
      filterHash: DoubleSha256DigestBE,
      prevHeaderHash: DoubleSha256DigestBE): FilterHeader = {
    new FilterHeader(filterHash.flip, prevHeaderHash.flip)
  }
}
