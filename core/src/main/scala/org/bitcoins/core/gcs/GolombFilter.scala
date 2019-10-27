package org.bitcoins.core.gcs

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.{UInt64, UInt8}
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.script.{EmptyScriptPubKey, ScriptPubKey}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.bitcoins.core.protocol.{CompactSizeUInt, NetworkElement}
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.util.{BitcoinSUtil, CryptoUtil}
import scodec.bits.{BitVector, ByteVector}

import scala.annotation.tailrec

/**
  * Represents a GCS encoded set with all parameters specified
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#golomb-coded-sets]]
  *
  * TODO: Replace ByteVector with a type for keys
  */
case class GolombFilter(
    key: SipHashKey,
    m: UInt64,
    p: UInt8,
    n: CompactSizeUInt,
    encodedData: BitVector)
    extends NetworkElement {
  lazy val f: UInt64 = n.num * m

  /** The hash of this serialized filter */
  lazy val hash: DoubleSha256Digest = {
    CryptoUtil.doubleSHA256(this.bytes)
  }

  /** Given the previous FilterHeader, constructs the header corresponding to this */
  def getHeader(prevHeader: FilterHeader): FilterHeader = {
    FilterHeader(filterHash = this.hash, prevHeaderHash = prevHeader.hash)
  }

  /** Given the previous FilterHeader hash, constructs the header corresponding to this */
  def getHeader(prevHeaderHash: DoubleSha256Digest): FilterHeader = {
    FilterHeader(filterHash = this.hash, prevHeaderHash = prevHeaderHash)
  }

  override def bytes: ByteVector = {
    n.bytes ++ encodedData.bytes
  }

  def hashToRange(item: ByteVector): UInt64 = GCS.hashToRange(item, f, key)

}

trait BlockFilterMatcher {

  /**
    * Checks if the underlying filter matches the given data
    */
  def matches(data: ByteVector): Boolean

  /**
    * Checks if the underlying filter matches any item from the given collection
    */
  def matchesAny(data: Vector[ByteVector]): Boolean
}

class SimpleFilterMatcher(filter: GolombFilter) extends BlockFilterMatcher {

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

class BinarySearchFilterMatcher(filter: GolombFilter)
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

object BlockFilter {

  /**
    * Returns all ScriptPubKeys from a Block's outputs that are relevant
    * to BIP 158 Basic Block Filters
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#contents]]
    */
  def getOutputScriptPubKeysFromBlock(block: Block): Vector[ScriptPubKey] = {
    val transactions: Vector[Transaction] = block.transactions.toVector

    val newOutputs: Vector[TransactionOutput] = transactions.flatMap(_.outputs)

    newOutputs
      .filterNot(_.scriptPubKey.asm.contains(OP_RETURN))
      .filterNot(_.scriptPubKey == EmptyScriptPubKey)
      .map(_.scriptPubKey)
  }

  /**
    * Returns all ScriptPubKeys from a Block's inputs that are relevant
    * to BIP 158 Basic Block Filters
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#contents]]
    */
  /*
  TODO uncomment and add unit tests for this method
  def getInputScriptPubKeysFromBlock(
      block: Block,
      utxoProvider: TempUtxoProvider): Vector[ScriptPubKey] = {
    val transactions: Vector[Transaction] = block.transactions.toVector
    val noCoinbase: Vector[Transaction] = transactions.tail

    val inputs: Vector[TransactionInput] = noCoinbase.flatMap(_.inputs)
    val outpointsSpent: Vector[TransactionOutPoint] =
      inputs.map(_.previousOutput)
    val prevOutputs: Vector[TransactionOutput] =
      outpointsSpent.flatMap(utxoProvider.getUtxo)

    prevOutputs
      .filterNot(_.scriptPubKey == EmptyScriptPubKey)
      .map(_.scriptPubKey)
  }
   */

  /**
    * Given a Block and access to the UTXO set, constructs a Block Filter for that block
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#block-filters]]
    */
  /*
  TODO uncomment and add unit tests for this method
  def apply(block: Block, utxoProvider: TempUtxoProvider): GolombFilter = {
    val prevOutputScripts: Vector[ScriptPubKey] =
      getInputScriptPubKeysFromBlock(block, utxoProvider)

    BlockFilter(block, prevOutputScripts)
  }
   */

  /**
    * Given a Block and access to the previous output scripts, constructs a Block Filter for that block
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#block-filters]]
    */
  def apply(
      block: Block,
      prevOutputScripts: Vector[ScriptPubKey]): GolombFilter = {
    val keyBytes: ByteVector = block.blockHeader.hash.bytes.take(16)

    val key: SipHashKey = SipHashKey(keyBytes)

    val newScriptPubKeys: Vector[ByteVector] =
      getOutputScriptPubKeysFromBlock(block).map(_.asmBytes)

    val prevOutputScriptBytes: Vector[ByteVector] =
      prevOutputScripts
        .filterNot(_ == EmptyScriptPubKey)
        .map(_.asmBytes)

    val allOutputs = (prevOutputScriptBytes ++ newScriptPubKeys).distinct

    GCS.buildBasicBlockFilter(allOutputs, key)
  }

  def fromBytes(
      bytes: ByteVector,
      blockHash: DoubleSha256Digest): GolombFilter = {
    val n = CompactSizeUInt.fromBytes(bytes)
    val filterBytes = bytes.drop(n.bytes.length)
    val keyBytes: ByteVector = blockHash.bytes.take(16)
    val key: SipHashKey = SipHashKey(keyBytes)

    GolombFilter(key,
                 FilterType.Basic.M,
                 FilterType.Basic.P,
                 n,
                 filterBytes.toBitVector)
  }

  def fromHex(hex: String, blockHash: DoubleSha256Digest): GolombFilter = {
    fromBytes(BitcoinSUtil.decodeHex(hex), blockHash)
  }
}
