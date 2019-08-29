package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.p2p.GetCompactFilterHeadersMessage
import scodec.bits.ByteVector

/**
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#getcfheaders BIP157]]
  */
object RawGetCompactFilterHeadersMessageSerializer
    extends RawBitcoinSerializer[GetCompactFilterHeadersMessage] {
  def read(bytes: ByteVector): GetCompactFilterHeadersMessage = ???

  def write(message: GetCompactFilterHeadersMessage): ByteVector =
    message.filterType.bytes ++ message.startHeight.bytes.reverse ++ message.stopHash.bytes
}
