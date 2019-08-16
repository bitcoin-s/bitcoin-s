package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.p2p.CompactFilterCheckPointMessage
import scodec.bits.ByteVector

object RawCompactFilterCheckpointMessageSerializer
    extends RawBitcoinSerializer[CompactFilterCheckPointMessage] {
  def read(bytes: ByteVector): CompactFilterCheckPointMessage = ???
  def write(message: CompactFilterCheckPointMessage): ByteVector = {
    message.filterType.bytes ++ message.stophash.bytes ++ message.filterHeadersLength.bytes ++ message.filterHeaders.foldLeft(ByteVector.empty)(_ ++ _.hash.bytes)
  }
}
