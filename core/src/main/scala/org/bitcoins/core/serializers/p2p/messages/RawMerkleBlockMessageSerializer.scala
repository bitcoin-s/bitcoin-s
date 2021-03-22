package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.p2p.MerkleBlockMessage
import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.core.serializers.RawBitcoinSerializer
import scodec.bits.ByteVector

/** Responsible for serialization and deserialization of MerkleBlockMessages
  * @see https://bitcoin.org/en/developer-reference#merkleblock
  */
trait RawMerkleBlockMessageSerializer
    extends RawBitcoinSerializer[MerkleBlockMessage] {

  def read(bytes: ByteVector): MerkleBlockMessage = {
    val merkleBlock = MerkleBlock(bytes)
    MerkleBlockMessage(merkleBlock)
  }

  def write(merkleBlockMessage: MerkleBlockMessage): ByteVector =
    merkleBlockMessage.merkleBlock.bytes

}

object RawMerkleBlockMessageSerializer extends RawMerkleBlockMessageSerializer
