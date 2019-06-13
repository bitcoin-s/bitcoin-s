package org.bitcoins.core.gcs

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.{UInt64, UInt8}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionInput,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.util.{BitcoinSUtil, BitcoinScriptUtil}
import scodec.bits.{BitVector, ByteVector}

import scala.annotation.tailrec

// TODO: Replace ByteVector with a type for keys
case class GolombFilter(
    key: ByteVector,
    m: UInt64,
    p: UInt8,
    n: CompactSizeUInt,
    encodedData: BitVector) {
  lazy val decodedHashes: Vector[UInt64] = GCS.golombDecodeSet(encodedData, p)

  // TODO: Offer alternative that stops decoding when it finds out if data is there
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

    binarySearch(from = 0, to = n.toInt - 1, hash, decodedHashes)
  }

  def matches(data: ByteVector): Boolean = {
    val f = n.num * m
    val hash = GCS.hashToRange(data, f, key)

    matchesHash(hash)
  }
}

object BlockFilter {

  val M: UInt64 = UInt64(784931)
  val P: UInt8 = UInt8(19)

  /**
    * Given a Block and access to the UTXO set, constructs a Block Filter for that block
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#block-filters]]
    */
  def apply(block: Block, utxoProvider: TempUtxoProvider): GolombFilter = {
    val key = block.blockHeader.hash.bytes.take(16)

    val transactions: Vector[Transaction] = block.transactions.toVector

    val noCoinbase: Vector[Transaction] = transactions.tail
    val newOutputs: Vector[TransactionOutput] = transactions.flatMap(_.outputs)
    val newScriptPubKeys: Vector[ByteVector] = newOutputs.flatMap { output =>
      if (output.scriptPubKey.asm.contains(OP_RETURN)) {
        None
      } else {
        Some(output.scriptPubKey.asmBytes)
      }
    }

    val inputs: Vector[TransactionInput] = noCoinbase.flatMap(tx => tx.inputs)
    val outpointsSpent: Vector[TransactionOutPoint] = inputs.map { input =>
      input.previousOutput
    }
    val prevOutputs: Vector[TransactionOutput] =
      outpointsSpent.flatMap(utxoProvider.getUtxo)
    val prevOutputScripts: Vector[ByteVector] =
      prevOutputs.map(_.scriptPubKey.asmBytes)

    GCS.buildBasicBlockFilter(prevOutputScripts ++ newScriptPubKeys, key)
  }

  /**
    * Given a Block and access to the previous output scripts, constructs a Block Filter for that block
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#block-filters]]
    */
  def apply(
      block: Block,
      prevOutputScripts: Vector[ScriptPubKey]): GolombFilter = {
    val key = block.blockHeader.hash.bytes.take(16)

    val transactions: Vector[Transaction] = block.transactions.toVector

    val newOutputs: Vector[TransactionOutput] = transactions.flatMap(_.outputs)
    val newScriptPubKeys: Vector[ByteVector] = newOutputs
      .filterNot(_.scriptPubKey.asm.contains(OP_RETURN))
      .map(_.scriptPubKey.asmBytes)

    val prevOutputScriptBytes: Vector[ByteVector] =
      prevOutputScripts.map(_.asmBytes)

    GCS.buildBasicBlockFilter(prevOutputScriptBytes ++ newScriptPubKeys, key)
  }

  def fromBytes(
      bytes: ByteVector,
      blockHash: DoubleSha256Digest): GolombFilter = {
    val (size, filterBytes) = bytes.splitAt(1)
    val n = CompactSizeUInt.fromBytes(size)

    GolombFilter(blockHash.bytes.take(16),
                 BlockFilter.M,
                 BlockFilter.P,
                 n,
                 filterBytes.toBitVector)
  }

  def fromHex(hex: String, blockHash: DoubleSha256Digest): GolombFilter = {
    fromBytes(BitcoinSUtil.decodeHex(hex), blockHash)
  }
}
