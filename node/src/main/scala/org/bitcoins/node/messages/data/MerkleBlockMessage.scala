package org.bitcoins.node.messages.data

import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.core.util.Factory
import org.bitcoins.node.messages.MerkleBlockMessage
import org.bitcoins.node.serializers.messages.data.RawMerkleBlockMessageSerializer
import org.bitcoins.node.messages.MerkleBlockMessage
import org.bitcoins.node.serializers.messages.data.RawMerkleBlockMessageSerializer
import scodec.bits.ByteVector

/**
  * Created by chris on 6/2/16.
  * https://bitcoin.org/en/developer-reference#merkleblock
  */
object MerkleBlockMessage extends Factory[MerkleBlockMessage] {

  private case class MerkleBlockMessageImpl(merkleBlock: MerkleBlock)
      extends MerkleBlockMessage

  def fromBytes(bytes: ByteVector): MerkleBlockMessage =
    RawMerkleBlockMessageSerializer.read(bytes)

  def apply(merkleBlock: MerkleBlock): MerkleBlockMessage = {
    MerkleBlockMessageImpl(merkleBlock)
  }
}
