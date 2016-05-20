package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.transaction.Transaction

/**
  * Created by chris on 5/19/16.
  * Represents a block in our blockchain
  * Bitcoin Core implementation:
  * https://github.com/bitcoin/bitcoin/blob/master/src/primitives/block.h#L73
  * Bitcoin Developer Reference link:
  * https://bitcoin.org/en/developer-reference#serialized-blocks
  */
trait Block {

  /**
    * The block header for this block
    * @return
    */
  def blockHeader : BlockHeader

  /**
    * The total number of transactions in this block,
    * including the coinbase transaction.
    * @return
    */
  def txCount : CompactSizeUInt

  /**
    * The transactions contained in this block
    * @return
    */
  def transactions : Seq[Transaction]

}


/**
  * Companion object for creating Blocks
  */
object Block {

  private sealed case class BlockImpl(blockHeader : BlockHeader,
    txCount : CompactSizeUInt, transactions : Seq[Transaction]) extends Block

  def apply(blockHeader : BlockHeader, txCount : CompactSizeUInt, transactions : Seq[Transaction]) : Block = {
    BlockImpl(blockHeader, txCount, transactions)
  }
}