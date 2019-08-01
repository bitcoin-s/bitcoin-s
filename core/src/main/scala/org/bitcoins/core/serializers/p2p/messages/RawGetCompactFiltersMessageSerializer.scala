package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.p2p.GetCompactFiltersMessage
import scodec.bits.ByteVector
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.crypto.DoubleSha256Digest

object RawGetCompactFiltersMessageSerializer
    extends RawBitcoinSerializer[GetCompactFiltersMessage] {

  def read(bytes: ByteVector): GetCompactFiltersMessage = {
    val filterType = FilterType.fromBytes(bytes.take(1))
    val (startHeightBytes, rest) = bytes.drop(1).splitAt(5)
    val startHeight = UInt32.fromBytes(startHeightBytes)
    val stopHash = DoubleSha256Digest.fromBytes(rest.take(32))
    GetCompactFiltersMessage(filterType, startHeight, stopHash)
  }

  def write(message: GetCompactFiltersMessage): ByteVector =
    message.filterType.bytes ++ message.startHeight.bytes ++ message.stopHash.bytes

}
