package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.p2p.GetCompactFiltersMessage
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.crypto.DoubleSha256Digest
import scodec.bits.ByteVector

/** @see [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#getcfilters BIP157]]
  */
object RawGetCompactFiltersMessageSerializer
    extends RawBitcoinSerializer[GetCompactFiltersMessage] {

  def read(bytes: ByteVector): GetCompactFiltersMessage = {
    val filterType = FilterType.fromBytes(bytes.take(1))
    val (startHeightBytes, rest) = bytes.drop(1).splitAt(4)
    val startHeight = UInt32.fromBytes(startHeightBytes.reverse)
    val stopHash = DoubleSha256Digest.fromBytes(rest.take(32))
    GetCompactFiltersMessage(filterType, startHeight, stopHash)
  }

  def write(message: GetCompactFiltersMessage): ByteVector = {
    val filterType = message.filterType.bytes
    val startHeight = message.startHeight.bytes.reverse
    val stopHash = message.stopHash.bytes
    val bytes = filterType ++ startHeight ++ stopHash
    bytes
  }

}
