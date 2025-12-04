package org.bitcoins.core.consensus
import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.core.protocol.blockchain.Block

trait Validation {

  def contextualCheckBlock(block: Block, tip: BlockHeaderDb): Boolean = {
    require(block.blockHeader.previousBlockHashBE == tip.hashBE)
    val nHeight = tip.height + 1
//    val expectedBits = Pow.calculateNextWorkRequired(
//      previousBlockHeader = tip.blockHeader,
//      newBlockTime = block.blockHeader.time,
//      params = tip.params)
    ???
  }
}
