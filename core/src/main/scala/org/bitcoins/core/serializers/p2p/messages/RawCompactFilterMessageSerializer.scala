package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.CompactSizeUInt
import scodec.bits.ByteVector

/**
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#cfilter BIP157]]
  */
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

  def write(message: CompactFilterMessage): ByteVector = {
    val filterType = message.filterType.bytes
    val blockHash = message.blockHash.bytes
    val numFilterBytes = CompactSizeUInt(UInt64(message.filterBytes.size)).bytes
    val filterBytes = message.filterBytes
    filterType ++ blockHash ++ numFilterBytes ++ filterBytes
  }
}
