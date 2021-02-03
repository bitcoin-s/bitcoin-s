package org.bitcoins.core.gcs

import org.bitcoins.core.number.UInt64
import scodec.bits.ByteVector

import scala.annotation.tailrec

sealed trait BlockFilterMatcher {

  /** Checks if the underlying filter matches the given data
    */
  def matches(data: ByteVector): Boolean

  /** Checks if the underlying filter matches any item from the given collection
    */
  def matchesAny(data: Vector[ByteVector]): Boolean
}

case class SimpleFilterMatcher(filter: GolombFilter)
    extends BlockFilterMatcher {

  override def matches(data: ByteVector): Boolean = {
    val hash = filter.hashToRange(data)
    matchesHash(hash)
  }

  /** Hashes the given vector of data and calls [[matchesAnyHash()]] to find a match */
  override def matchesAny(data: Vector[ByteVector]): Boolean = {
    val hashes = data.map(filter.hashToRange)
    matchesAnyHash(hashes)
  }

  def matchesHash(hash: UInt64): Boolean = {
    var matches = false
    GCS.golombDecodeSetsWithPredicate(filter.encodedData, filter.p) {
      decodedHash =>
        if (hash > decodedHash) {
          true
        } else {
          if (hash == decodedHash) {
            matches = true
          }
          false
        }
    }
    matches
  }

  /** It implements https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#golomb-coded-set-multi-match */
  def matchesAnyHash(hashes: Vector[UInt64]): Boolean = {
    val sortedHashes = hashes.sorted
    var matches = false
    var i = 0

    def predicate(decodedHash: UInt64): Boolean = {
      while (i < sortedHashes.size) {
        val hash = sortedHashes(i)
        if (hash == decodedHash) {
          matches = true
          return false
        } else if (hash > decodedHash) {
          return true
        } else {
          i += 1
        }
      }
      false
    }

    GCS.golombDecodeSetsWithPredicate(filter.encodedData, filter.p)(predicate)

    matches
  }

}

case class BinarySearchFilterMatcher(filter: GolombFilter)
    extends BlockFilterMatcher {

  lazy val decodedHashes: Vector[UInt64] =
    GCS.golombDecodeSet(filter.encodedData, filter.p)

  override def matches(data: ByteVector): Boolean = {
    val hash = filter.hashToRange(data)

    matchesHash(hash)
  }

  /** Hashes the given vector of data and calls [[matchesAnyHash()]] to find a match */
  override def matchesAny(data: Vector[ByteVector]): Boolean = {
    val hashes = data.map(filter.hashToRange)
    matchesAnyHash(hashes)
  }

  def matchesHash(hash: UInt64): Boolean = {
    @tailrec
    def binarySearch(
        from: Int,
        to: Int,
        hash: UInt64,
        set: Vector[UInt64]): Boolean = {
      if (to < from) {
        false
      } else {
        val index = (to + from) / 2
        val otherHash = set(index)

        if (hash == otherHash) {
          true
        } else if (hash < otherHash) {
          binarySearch(from, index - 1, hash, set)
        } else {
          binarySearch(index + 1, to, hash, set)
        }
      }
    }

    binarySearch(from = 0, to = filter.n.toInt - 1, hash, decodedHashes)
  }

  /** Checks whether there's a match for at least one of the given hashes
    */
  def matchesAnyHash(hashes: Vector[UInt64]): Boolean =
    hashes.exists(matchesHash)

}
