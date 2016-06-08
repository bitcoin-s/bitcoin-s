package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.{CompactSizeUInt, NetworkElement}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.serializers.blockchain.RawBlockSerializer
import org.bitcoins.core.util.{BitcoinSLogger, CryptoUtil, Factory}

/**
  * Created by chris on 5/19/16.
  * Represents a block in our blockchain
  * Bitcoin Core implementation:
  * https://github.com/bitcoin/bitcoin/blob/master/src/primitives/block.h#L73
  * Bitcoin Developer Reference link:
  * https://bitcoin.org/en/developer-reference#serialized-blocks
  */
sealed trait Block extends NetworkElement with BitcoinSLogger {

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

  override def hex = RawBlockSerializer.write(this)

}


/**
  * Companion object for creating Blocks
  */
object Block extends Factory[Block] {

  private sealed case class BlockImpl(blockHeader : BlockHeader,
    txCount : CompactSizeUInt, transactions : Seq[Transaction]) extends Block

  def apply(blockHeader : BlockHeader, txCount : CompactSizeUInt, transactions : Seq[Transaction]) : Block = {
    BlockImpl(blockHeader, txCount, transactions)
  }

  def fromBytes(bytes : Seq[Byte]) : Block = RawBlockSerializer.read(bytes)
}