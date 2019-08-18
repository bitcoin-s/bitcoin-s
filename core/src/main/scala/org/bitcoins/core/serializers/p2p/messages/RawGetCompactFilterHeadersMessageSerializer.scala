package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.p2p.GetCompactFilterHeadersMessage
import scodec.bits.ByteVector

object RawGetCompactFilterHeadersMessageSerializer
    extends RawBitcoinSerializer[GetCompactFilterHeadersMessage] {
  def read(bytes: ByteVector): GetCompactFilterHeadersMessage = ???

  def write(message: GetCompactFilterHeadersMessage): ByteVector =
    message.filterType.bytes ++ message.startHeight.bytes.reverse ++ message.stopHash.bytes
}
