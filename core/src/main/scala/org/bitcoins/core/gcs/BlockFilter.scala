package org.bitcoins.core.gcs

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.script.{EmptyScriptPubKey, ScriptPubKey}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.util.BitcoinSUtil
import scodec.bits.ByteVector

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
