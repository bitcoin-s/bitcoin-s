package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.transaction.{ BaseTransaction, Transaction, WitnessTransaction }
import org.bitcoins.core.protocol.{ CompactSizeUInt, NetworkElement }
import org.bitcoins.core.serializers.blockchain.RawBlockSerializer
import org.bitcoins.core.util.{ BitcoinSLogger, Factory }

/**
 * Created by chris on 5/19/16.
 * Represents a block in our blockchain
 * Bitcoin Core implementation:
 * [[https://github.com/bitcoin/bitcoin/blob/master/src/primitives/block.h#L73]]
 * Bitcoin Developer Reference link:
 * [[https://bitcoin.org/en/developer-reference#serialized-blocks]]
 */
sealed abstract class Block extends NetworkElement {

  /** The block header for this block */
  def blockHeader: BlockHeader

  /**
   * The total number of transactions in this block,
   * including the coinbase transaction.
   */
  def txCount: CompactSizeUInt

  /** The transactions contained in this block */
  def transactions: Seq[Transaction]

  override def bytes = RawBlockSerializer.write(this)

  /**
   * This is the new computation to determine the maximum size of a block as per BIP141
   * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#block-size]]
   * The weight of a block is determined as follows:
   *
   * Base size is the block size in bytes with the original transaction serialization without any witness-related data
   *
   * Total size is the block size in bytes with transactions serialized as described in BIP144, including base data and witness data.
   *
   * Block weight is defined as Base size * 3 + Total size
   * [[https://github.com/bitcoin/bitcoin/blob/7490ae8b699d2955b665cf849d86ff5bb5245c28/src/primitives/block.cpp#L35]]
   */
  def blockWeight: Long = transactions.map(_.weight).sum
}

/**
 * Companion object for creating Blocks
 */
object Block extends Factory[Block] {

  private sealed case class BlockImpl(
    blockHeader: BlockHeader,
    txCount:     CompactSizeUInt, transactions: Seq[Transaction]
  ) extends Block

  def apply(blockHeader: BlockHeader, txCount: CompactSizeUInt, transactions: Seq[Transaction]): Block = {
    BlockImpl(blockHeader, txCount, transactions)
  }

  def apply(blockHeader: BlockHeader, transactions: Seq[Transaction]): Block = {
    val txCount = CompactSizeUInt(UInt64(transactions.size))
    Block(blockHeader, txCount, transactions)
  }

  def fromBytes(bytes: Seq[Byte]): Block = RawBlockSerializer.read(bytes)

}