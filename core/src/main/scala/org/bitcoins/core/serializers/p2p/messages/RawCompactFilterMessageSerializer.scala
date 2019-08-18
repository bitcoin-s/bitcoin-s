package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.CompactSizeUInt
import scodec.bits.ByteVector

object RawCompactFilterMessageSerializer
    extends RawBitcoinSerializer[CompactFilterMessage] {

  def read(bytes: ByteVector): CompactFilterMessage = {
    val filterType = FilterType.fromBytes(bytes.take(1))

    val (blockHashBytes, afterBlockHash) = bytes.drop(1).splitAt(32)
    val blockHash = DoubleSha256Digest.fromBytes(blockHashBytes)

    val mumFilterBytes = CompactSizeUInt.parse(afterBlockHash)

    val filterBytes = afterBlockHash
      .drop(mumFilterBytes.bytes.length)
      .take(mumFilterBytes.toInt)

    CompactFilterMessage(filterType, blockHash, filterBytes)
  }

  def write(message: CompactFilterMessage): ByteVector =
    message.filterType.bytes ++ message.blockHash.bytes ++ message.numFilterBytes.bytes ++ message.filterBytes
}
