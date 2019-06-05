package org.bitcoins.node.serializers.messages

import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.messages.MerkleBlockMessage
import scodec.bits.ByteVector

/**
  * Responsible for serialization and deserialization of MerkleBlockMessages
  * @see https://bitcoin.org/en/developer-reference#merkleblock
  */
trait RawMerkleBlockMessageSerializer
    extends RawBitcoinSerializer[MerkleBlockMessage]
    with BitcoinSLogger {

  def read(bytes: ByteVector): MerkleBlockMessage = {
    val merkleBlock = MerkleBlock(bytes)
    MerkleBlockMessage(merkleBlock)
  }

  def write(merkleBlockMessage: MerkleBlockMessage): ByteVector =
    merkleBlockMessage.merkleBlock.bytes

}

object RawMerkleBlockMessageSerializer extends RawMerkleBlockMessageSerializer
