package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.p2p.GetCompactFilterHeadersMessage
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.crypto.DoubleSha256Digest
import scodec.bits.ByteVector

/** @see [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#getcfheaders BIP157]]
  */
object RawGetCompactFilterHeadersMessageSerializer
    extends RawBitcoinSerializer[GetCompactFilterHeadersMessage] {

  def read(bytes: ByteVector): GetCompactFilterHeadersMessage = {
    val filterType = FilterType.fromBytes(bytes.take(1))
    val startHeight = UInt32.fromBytes(bytes.drop(1).take(4).reverse)
    val stopHash = bytes.drop(5).take(32)
    GetCompactFilterHeadersMessage(
      filterType = filterType,
      startHeight = startHeight,
      stopHash = DoubleSha256Digest.fromBytes(stopHash)
    )
  }

  def write(message: GetCompactFilterHeadersMessage): ByteVector = {
    val filterType = message.filterType.bytes
    val startHeight = message.startHeight.bytes.reverse
    val stopHash = message.stopHash.bytes
    val bytes = filterType ++ startHeight ++ stopHash
    bytes
  }
}
