package org.bitcoins.node.serializers.messages.data

import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.node.messages.BlockMessage
import org.bitcoins.node.messages.data.BlockMessage
import org.bitcoins.node.messages.BlockMessage
import org.bitcoins.node.messages.data.BlockMessage
import scodec.bits.ByteVector

/**
  * Created by chris on 7/8/16.
  */
trait RawBlockMessageSerializer extends RawBitcoinSerializer[BlockMessage] {

  def read(bytes: ByteVector): BlockMessage = {
    val block = Block.fromBytes(bytes)
    BlockMessage(block)
  }

  def write(blockMsg: BlockMessage): ByteVector = blockMsg.block.bytes
}

object RawBlockMessageSerializer extends RawBlockMessageSerializer
