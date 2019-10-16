package org.bitcoins.core.gcs

import org.bitcoins.core.number.{UInt64, UInt8}
import scodec.bits.BitVector

import scala.annotation.tailrec

case class GCSHashCache(encodedData: BitVector, p: UInt8) {
  // TODO: Make this a mutable collection
  /** Should only be appended to */
  var decodedHashes: Vector[UInt64] = Vector.empty

  /** Should only increase */
  var encodedIndex: Int = 0

  /** Should only increase */
  var lastHash: UInt64 = UInt64.zero

  private def doneDecoding: Boolean = {
    if (encodedData.length - encodedIndex < p.toInt + 1) {
      // Only padding left
      encodedIndex = encodedData.length.toInt
      true
    } else {
      false
    }
  }

  /** Async safe because decodedHashes is append-only (and sorted) */
  @tailrec
  private def binarySearchDecodedHashes(
      hash: UInt64,
      lowerIndex: Int = 0,
      upperIndex: Int = decodedHashes.length - 1): Option[Int] = {
    if (lowerIndex > upperIndex) {
      None
    } else {
      val mid = (lowerIndex + upperIndex) / 2
      val midHash = decodedHashes(mid)
      if (midHash > hash) {
        binarySearchDecodedHashes(hash, lowerIndex, mid - 1)
      } else if (midHash == hash) {
        Some(mid)
      } else {
        binarySearchDecodedHashes(hash, mid + 1, upperIndex)
      }
    }
  }

  private def decodeOneHashIfPossible(): Unit = {
    if (!doneDecoding) {
      val delta = GCS.golombDecode(encodedData.drop(encodedIndex), p)
      val prefixSize = (delta >> p.toInt).toInt + 1
      val newHash = lastHash + delta

      decodedHashes = decodedHashes.:+(newHash)
      encodedIndex += prefixSize + p.toInt
      lastHash = newHash
    }
  }

  @tailrec
  final def contains(hash: UInt64): Boolean = {
    if (hash == lastHash) {
      true
    } else if (hash < lastHash) {
      binarySearchDecodedHashes(hash).isDefined
    } else {
      if (doneDecoding && hash > lastHash) {
        false
      } else {
        decodeOneHashIfPossible()

        // We call contains rather than checking lastHash to be async safe
        contains(hash)
      }
    }
  }

  /** Implements https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#golomb-coded-set-multi-match */
  def containsAny(hashes: Vector[UInt64], lowerIndex: Int = 0): Boolean = {
    containsAnySorted(hashes.sortBy(_.toLong), lowerIndex)
  }

  @tailrec
  final def containsAnySorted(
      sortedHashes: Vector[UInt64],
      lowerIndex: Int = 0): Boolean = {

    @tailrec
    def matchNext(
        hash: UInt64,
        indexToCheck: Int = lowerIndex): (Boolean, Int) = {
      if (indexToCheck >= decodedHashes.length) {
        if (doneDecoding) {
          (false, indexToCheck)
        } else {
          val diff = indexToCheck - decodedHashes.length
          (0 until diff).foreach(_ => decodeOneHashIfPossible())

          // We call matchNext rather than checking lastHash to be async safe
          matchNext(hash, indexToCheck)
        }
      } else {
        val nextHash = decodedHashes(indexToCheck)
        if (hash == nextHash) {
          (true, indexToCheck + 1)
        } else if (hash < nextHash) {
          matchNext(hash, indexToCheck + 1)
        } else {
          (false, indexToCheck)
        }
      }
    }

    if (sortedHashes.isEmpty) {
      true
    } else {
      val (matched, newLowerIndex) = matchNext(sortedHashes.head)

      if (matched) {
        true
      } else {
        containsAnySorted(sortedHashes.tail, newLowerIndex)
      }
    }
  }

  lazy val allHashes: Vector[UInt64] = {
    @tailrec
    def decodeRemaining(): Unit = {
      if (!doneDecoding) {
        decodeOneHashIfPossible()
        decodeRemaining()
      }
    }

    decodeRemaining()

    decodedHashes
  }
}
