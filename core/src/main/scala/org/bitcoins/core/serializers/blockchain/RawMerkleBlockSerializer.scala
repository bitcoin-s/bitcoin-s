package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.BitcoinSUtil
import scodec.bits.{BitVector, ByteVector}

import scala.annotation.tailrec

/**
  * Created by chris on 8/15/16.
  * [[https://bitcoin.org/en/developer-reference#merkleblock]]
  */
sealed abstract class RawMerkleBlockSerializer
    extends RawBitcoinSerializer[MerkleBlock] {

  def read(bytes: ByteVector): MerkleBlock = {
    val (headerBytes, afterHeader) = bytes.splitAt(80)
    val blockHeader = RawBlockHeaderSerializer.read(headerBytes)

    val (txCountBytes, afterTxCount) = afterHeader.splitAt(4)

    val transactionCount = UInt32.fromBytes(txCountBytes.reverse)
    val hashCount = CompactSizeUInt.parseCompactSizeUInt(afterTxCount)

    val (_, afterHashCountBytes) = afterTxCount.splitAt(hashCount.bytes.length)

    val (hashes, bytesAfterTxHashParsing) =
      parseTransactionHashes(afterHashCountBytes, hashCount)

    val flagCount =
      CompactSizeUInt.parseCompactSizeUInt(bytesAfterTxHashParsing)

    val (_, afterFlagCountBytes) =
      bytesAfterTxHashParsing.splitAt(flagCount.bytes.length)

    val flags = afterFlagCountBytes.take(flagCount.toInt)

    val matches = flags.toArray
      .map(BitVector(_).reverse)
      .foldLeft(BitVector.empty)(_ ++ _)

    MerkleBlock(blockHeader, transactionCount, hashes, matches)
  }

  def write(merkleBlock: MerkleBlock): ByteVector = {
    val partialMerkleTree = merkleBlock.partialMerkleTree
    val bitVectors = partialMerkleTree.bits
    val byteVectors: ByteVector = {
      bitVectors.toByteArray
        .map(BitVector(_).reverse)
        .foldLeft(ByteVector.empty)(_ ++ _.bytes)
    }
    val flagCount = CompactSizeUInt(
      UInt64(Math.ceil(partialMerkleTree.bits.size.toDouble / 8).toInt))
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
  private def parseTransactionHashes(
      bytes: ByteVector,
      hashCount: CompactSizeUInt): (Seq[DoubleSha256Digest], ByteVector) = {
    @tailrec
    def loop(
        remainingHashes: Long,
        remainingBytes: ByteVector,
        accum: List[DoubleSha256Digest]): (Seq[DoubleSha256Digest], ByteVector) = {
      if (remainingHashes <= 0) (accum.reverse, remainingBytes)
      else {
        val (hashBytes, newRemainingBytes) = remainingBytes.splitAt(32)
        loop(remainingHashes - 1,
             newRemainingBytes,
             DoubleSha256Digest(hashBytes) :: accum)
      }
    }
    loop(hashCount.num.toInt, bytes, Nil)
  }
}

object RawMerkleBlockSerializer extends RawMerkleBlockSerializer
