package org.bitcoins.node.serializers.messages

import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.node.messages.BlockMessage
import scodec.bits.ByteVector

trait RawBlockMessageSerializer extends RawBitcoinSerializer[BlockMessage] {

  def read(bytes: ByteVector): BlockMessage = {
    val block = Block.fromBytes(bytes)
    BlockMessage(block)
  }

  def write(blockMsg: BlockMessage): ByteVector = blockMsg.block.bytes
}

object RawBlockMessageSerializer extends RawBlockMessageSerializer
