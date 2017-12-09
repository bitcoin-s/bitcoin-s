package org.bitcoins.core.serializers.blockchain


import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.BitcoinSUtil

import scala.annotation.tailrec

/**
  * Created by chris on 8/15/16.
  * [[https://bitcoin.org/en/developer-reference#merkleblock]]
  */
trait RawMerkleBlockSerializer extends RawBitcoinSerializer[MerkleBlock] {

  def read(bytes: List[Byte]): MerkleBlock = {
    val blockHeader = RawBlockHeaderSerializer.read(bytes.take(80))
    val bytesAfterBlockHeaderParsing = bytes.slice(blockHeader.bytes.size, bytes.size)
    val transactionCount = UInt32(bytesAfterBlockHeaderParsing.slice(0,4).reverse)
    val hashCount = CompactSizeUInt.parseCompactSizeUInt(
      bytesAfterBlockHeaderParsing.slice(4,bytesAfterBlockHeaderParsing.size))
    val txHashStartIndex = (4 + hashCount.size).toInt
    val bytesAfterHashCountParsing = bytesAfterBlockHeaderParsing.slice(txHashStartIndex,bytesAfterBlockHeaderParsing.size)

    val (hashes, bytesAfterTxHashParsing) = parseTransactionHashes(bytesAfterHashCountParsing,hashCount)
    logger.debug("Bytes after tx hash parsing: " + BitcoinSUtil.encodeHex(bytesAfterTxHashParsing))
    val flagCount = CompactSizeUInt.parseCompactSizeUInt(bytesAfterTxHashParsing)
    val flags = bytesAfterTxHashParsing.slice(flagCount.size.toInt, bytesAfterTxHashParsing.size)
    logger.debug("Flags after parsing: " + BitcoinSUtil.encodeHex(flags))
    val matches = BitcoinSUtil.bytesToBitVectors(flags).flatMap(_.reverse)
    MerkleBlock(blockHeader,transactionCount,hashes,matches)
  }

  def write(merkleBlock: MerkleBlock): String = {
    val partialMerkleTree = merkleBlock.partialMerkleTree
    val bitVectors = parseToBytes(partialMerkleTree.bits)
    val byteVectors = BitcoinSUtil.bitVectorsToBytes(bitVectors)
    val flagCount = CompactSizeUInt(UInt64(Math.ceil(partialMerkleTree.bits.size.toDouble / 8).toInt))
    merkleBlock.blockHeader.hex +
      BitcoinSUtil.flipEndianness(merkleBlock.transactionCount.hex) +
      CompactSizeUInt(UInt64(merkleBlock.hashes.size)).hex + merkleBlock.hashes.map(_.hex).mkString +
      flagCount.hex + BitcoinSUtil.encodeHex(byteVectors)
  }


  /**
    * Parses a sequence of transactions hashes from inside of a merkle block message
    * @param bytes the bytes from which the tx hashes are parsed from
    * @param hashCount the amount of tx hashes we need to parse from bytes
    * @return the sequence of tx hashes and the remaining bytes to be parsed into a MerkleBlockMessage
    */
  private def parseTransactionHashes(bytes : Seq[Byte], hashCount : CompactSizeUInt) : (Seq[DoubleSha256Digest], Seq[Byte]) = {
    @tailrec
    def loop(remainingHashes : Long, remainingBytes : Seq[Byte],
             accum : List[DoubleSha256Digest]) : (Seq[DoubleSha256Digest], Seq[Byte]) = {
      if (remainingHashes <= 0) (accum.reverse,remainingBytes)
      else loop(remainingHashes-1, remainingBytes.slice(32,remainingBytes.size), DoubleSha256Digest(remainingBytes.take(32)) :: accum)
    }
    loop(hashCount.num.toInt, bytes, List())
  }


  /** Parses a sequence of bits to a sequence of bit vectors grouped into bytes */
  private def parseToBytes(bits: Seq[Boolean]): Seq[Seq[Boolean]] = {
    @tailrec
    def loop(remainingBits: Seq[Boolean], accum: Seq[Seq[Boolean]]): Seq[Seq[Boolean]] = remainingBits match {
      case Nil => accum.reverse
      case h :: t => accum.headOption match {
        case None => loop(remainingBits, Nil +: accum)
        case Some(bits) if bits.size == 8 =>
          //if we have 8 bits in this sequence we need to create a new byte and prepend it to the accum
          loop(remainingBits, Nil +: accum)
        case Some(bits) =>
          val newBits = h +: bits
          loop(t, newBits +: accum.tail)
      }
    }
    loop(bits,Seq(Seq()))
  }
}

object RawMerkleBlockSerializer extends RawMerkleBlockSerializer
