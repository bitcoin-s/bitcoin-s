package org.bitcoins.node.serializers.messages.data

import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.messages.MerkleBlockMessage
import org.bitcoins.node.messages.data.MerkleBlockMessage
import org.bitcoins.node.messages.MerkleBlockMessage
import scodec.bits.ByteVector

/**
  * Created by chris on 6/2/16.
  * Responsible for serialization and deserialization of MerkleBlockMessages
  * https://bitcoin.org/en/developer-reference#merkleblock
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
