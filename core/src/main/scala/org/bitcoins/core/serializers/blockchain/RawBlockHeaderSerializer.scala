package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.{ Int32, UInt32 }
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.serializers.RawBitcoinSerializer
import scodec.bits.ByteVector

/**
 * Created by chris on 5/19/16.
 * Serializes block headers
 * https://bitcoin.org/en/developer-reference#block-headers
 */
sealed abstract class RawBlockHeaderSerializer extends RawBitcoinSerializer[BlockHeader] {

  /** Converts a list of bytes into a block header */
  def read(bytes: ByteVector): BlockHeader = {
    //version first 4 bytes
    val version = Int32(bytes.take(4).reverse)
    //previous header hash next 32 bytes
    val prevBlockHashBytes = bytes.slice(4, 36)
    val prevBlockHash: DoubleSha256Digest = DoubleSha256Digest(prevBlockHashBytes)
    //merkle hash next 32 bytes
    val merkleRootBytes = bytes.slice(36, 68)
    val merkleRoot: DoubleSha256Digest = DoubleSha256Digest(merkleRootBytes)
    //time 4 bytes
    val timeBytes = bytes.slice(68, 72)
    val time = UInt32(timeBytes.reverse)
    //nbits 4 bytes
    val nBitsBytes = bytes.slice(72, 76)
    val nBits = UInt32(nBitsBytes.reverse)
    //nonce 4 bytes
    val nonceBytes = bytes.slice(76, 80)
    val nonce = UInt32(nonceBytes.reverse)
    BlockHeader(version, prevBlockHash, merkleRoot, time, nBits, nonce)
  }

  /** Serializes the BlockHeader to a byte array */
  def write(blockHeader: BlockHeader): ByteVector = {
    val version = blockHeader.version.bytes.reverse

    val prevHash = blockHeader.previousBlockHash.bytes
    val merkleRoot = blockHeader.merkleRootHash.bytes

    val time = blockHeader.time.bytes.reverse
    val nBits = blockHeader.nBits.bytes.reverse
    val nonce = blockHeader.nonce.bytes.reverse

    version ++ prevHash ++ merkleRoot ++ time ++ nBits ++ nonce
  }

}

object RawBlockHeaderSerializer extends RawBlockHeaderSerializer
