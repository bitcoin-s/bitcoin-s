package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.{ UInt32, UInt64 }
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.BitcoinSUtil
import org.slf4j.LoggerFactory
import scodec.bits.{ BitVector, ByteVector }

import scala.annotation.tailrec

/**
 * Created by chris on 8/15/16.
 * [[https://bitcoin.org/en/developer-reference#merkleblock]]
 */
sealed abstract class RawMerkleBlockSerializer extends RawBitcoinSerializer[MerkleBlock] {

  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  def read(bytes: scodec.bits.ByteVector): MerkleBlock = {
    val blockHeader = RawBlockHeaderSerializer.read(bytes.take(80))
    val bytesAfterBlockHeaderParsing = bytes.slice(blockHeader.bytes.size, bytes.size)
    val transactionCount = UInt32(bytesAfterBlockHeaderParsing.slice(0, 4).reverse)
    val hashCount = CompactSizeUInt.parseCompactSizeUInt(
      bytesAfterBlockHeaderParsing.slice(4, bytesAfterBlockHeaderParsing.size))
    val txHashStartIndex = (4 + hashCount.size).toInt
    val bytesAfterHashCountParsing = bytesAfterBlockHeaderParsing.slice(txHashStartIndex, bytesAfterBlockHeaderParsing.size)

    val (hashes, bytesAfterTxHashParsing) = parseTransactionHashes(bytesAfterHashCountParsing, hashCount)
    val flagCount = CompactSizeUInt.parseCompactSizeUInt(bytesAfterTxHashParsing)
    val flags = bytesAfterTxHashParsing.slice(flagCount.size.toInt, bytesAfterTxHashParsing.size)
    val matches = flags.toArray
      .map(BitVector(_).reverse)
      .foldLeft(BitVector.empty)(_ ++ _)
    MerkleBlock(blockHeader, transactionCount, hashes, matches)
  }

  def write(merkleBlock: MerkleBlock): scodec.bits.ByteVector = {
    val partialMerkleTree = merkleBlock.partialMerkleTree
    val bitVectors = partialMerkleTree.bits
    val byteVectors: scodec.bits.ByteVector = {
      bitVectors.toByteArray
        .map(BitVector(_).reverse)
        .foldLeft(ByteVector.empty)(_ ++ _.bytes)
    }
    val flagCount = CompactSizeUInt(UInt64(Math.ceil(partialMerkleTree.bits.size.toDouble / 8).toInt))
    val hashes: ByteVector = BitcoinSUtil.toByteVector(merkleBlock.hashes)
    merkleBlock.blockHeader.bytes ++
      merkleBlock.transactionCount.bytes.reverse ++
      CompactSizeUInt(UInt64(merkleBlock.hashes.size)).bytes ++
      hashes ++ flagCount.bytes ++ byteVectors
  }

  /**
   * Parses a sequence of transactions hashes from inside of a merkle block message
   * @param bytes the bytes from which the tx hashes are parsed from
   * @param hashCount the amount of tx hashes we need to parse from bytes
   * @return the sequence of tx hashes and the remaining bytes to be parsed into a MerkleBlockMessage
   */
  private def parseTransactionHashes(bytes: scodec.bits.ByteVector, hashCount: CompactSizeUInt): (Seq[DoubleSha256Digest], scodec.bits.ByteVector) = {
    @tailrec
    def loop(remainingHashes: Long, remainingBytes: scodec.bits.ByteVector,
      accum: List[DoubleSha256Digest]): (Seq[DoubleSha256Digest], scodec.bits.ByteVector) = {
      if (remainingHashes <= 0) (accum.reverse, remainingBytes)
      else loop(remainingHashes - 1, remainingBytes.slice(32, remainingBytes.size), DoubleSha256Digest(remainingBytes.take(32)) :: accum)
    }
    loop(hashCount.num.toInt, bytes, Nil)
  }
}

object RawMerkleBlockSerializer extends RawMerkleBlockSerializer
