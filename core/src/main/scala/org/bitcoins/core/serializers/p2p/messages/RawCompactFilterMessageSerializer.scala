package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.p2p.CompactFilterMessage
import scodec.bits.ByteVector

object RawCompactFilterMessageSerializer
    extends RawBitcoinSerializer[CompactFilterMessage] {

  def read(bytes: ByteVector): CompactFilterMessage = ???
  def write(t: CompactFilterMessage): ByteVector = ???
}
