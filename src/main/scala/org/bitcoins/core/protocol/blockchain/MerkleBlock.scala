package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.{ UInt32, UInt64 }
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.protocol.{ CompactSizeUInt, NetworkElement }
import org.bitcoins.core.serializers.blockchain.RawMerkleBlockSerializer
import org.bitcoins.core.util.Factory

import scala.annotation.tailrec

/**
 * Created by chris on 8/7/16.
 */
sealed abstract class MerkleBlock extends NetworkElement {

  /** The block header for the this merkle block */
  def blockHeader: BlockHeader

  /** The number of transactions in the original block */
  def transactionCount: UInt32

  /** The amount of hashes inside of the merkle block */
  def hashCount: CompactSizeUInt = CompactSizeUInt(UInt64(hashes.size))

  /** One or more hashes of both transactions and merkle nodes used to build the partial merkle tree */
  def hashes: Seq[DoubleSha256Digest] = partialMerkleTree.hashes

  /** The [[PartialMerkleTree]] for this merkle block */
  def partialMerkleTree: PartialMerkleTree

  def bytes = RawMerkleBlockSerializer.write(this)
}

object MerkleBlock extends Factory[MerkleBlock] {

  private case class MerkleBlockImpl(blockHeader: BlockHeader, transactionCount: UInt32,
                                     partialMerkleTree: PartialMerkleTree) extends MerkleBlock
  /**
   * Creates a [[MerkleBlock]] from the given [[Block]] and [[BloomFilter]]
   * This function iterates through each transaction inside our block checking if it is relevant to the given bloom filter
   * If it is relevant, it will set a flag to indicate we should include it inside of our [[PartialMerkleTree]]
   * @param block the block that we searching for transactions that match the bloom filter
   * @param filter the filter we are comparing transactions in the block against
   * @return the merkle block and the bloom filter loaded with information from the relevant txs in the block
   */
  def apply(block: Block, filter: BloomFilter): (MerkleBlock, BloomFilter) = {
    @tailrec
    def loop(remainingTxs: Seq[Transaction], accumFilter: BloomFilter,
             txMatches: Seq[(Boolean, DoubleSha256Digest)]): (Seq[(Boolean, DoubleSha256Digest)], BloomFilter) = {
      if (remainingTxs.isEmpty) (txMatches.reverse, accumFilter)
      else {
        val tx = remainingTxs.head
        val newTxMatches = (accumFilter.isRelevant(tx), tx.txId) +: txMatches
        val newFilter = accumFilter.update(tx)
        loop(remainingTxs.tail, newFilter, newTxMatches)
      }
    }
    val (matchedTxs, newFilter) = loop(block.transactions, filter, Nil)
    val partialMerkleTree = PartialMerkleTree(matchedTxs)
    val txCount = UInt32(block.transactions.size)
    (MerkleBlock(block.blockHeader, txCount, partialMerkleTree), newFilter)
  }

  /** Creates a merkle block that matches the given txids if they appear inside the given block */
  def apply(block: Block, txIds: Seq[DoubleSha256Digest]): MerkleBlock = {
    //follows this function inside of bitcoin core
    //https://github.com/bitcoin/bitcoin/blob/master/src/merkleblock.cpp#L40
    @tailrec
    def loop(remainingTxs: Seq[Transaction], txMatches: Seq[(Boolean, DoubleSha256Digest)]): (Seq[(Boolean, DoubleSha256Digest)]) = {
      if (remainingTxs.isEmpty) txMatches.reverse
      else {
        val tx = remainingTxs.head
        val newTxMatches = (txIds.contains(tx.txId), tx.txId) +: txMatches
        loop(remainingTxs.tail, newTxMatches)
      }
    }

    val txMatches = loop(block.transactions, Nil)

    val partialMerkleTree = PartialMerkleTree(txMatches)
    val txCount = UInt32(block.transactions.size)
    MerkleBlock(block.blockHeader, txCount, partialMerkleTree)
  }

  def apply(blockHeader: BlockHeader, txCount: UInt32,
            partialMerkleTree: PartialMerkleTree): MerkleBlock = {
    MerkleBlockImpl(blockHeader, txCount, partialMerkleTree)
  }

  def apply(blockHeader: BlockHeader, txCount: UInt32, hashes: Seq[DoubleSha256Digest], bits: Seq[Boolean]): MerkleBlock = {
    val partialMerkleTree = PartialMerkleTree(txCount, hashes, bits)
    MerkleBlock(blockHeader, txCount, partialMerkleTree)
  }

  def fromBytes(bytes: Seq[Byte]): MerkleBlock = RawMerkleBlockSerializer.read(bytes)
}
