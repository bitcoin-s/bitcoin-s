package org.bitcoins.node.messages.data

import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.util.Factory
import org.bitcoins.node.messages.BlockMessage
import org.bitcoins.node.serializers.messages.data.RawBlockMessageSerializer
import org.bitcoins.node.messages.BlockMessage
import scodec.bits.ByteVector

/**
  * Created by chris on 7/8/16.
  */
object BlockMessage extends Factory[BlockMessage] {

  private case class BlockMessageImpl(block: Block) extends BlockMessage

  def fromBytes(bytes: ByteVector): BlockMessage =
    RawBlockMessageSerializer.read(bytes)

  def apply(block: Block): BlockMessage = BlockMessageImpl(block)

}
