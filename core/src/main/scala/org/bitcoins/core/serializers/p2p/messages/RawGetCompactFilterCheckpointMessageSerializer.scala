package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.p2p.GetCompactFilterCheckPointMessage
import scodec.bits.ByteVector

object RawGetCompactFilterCheckpointMessageSerializer
    extends RawBitcoinSerializer[GetCompactFilterCheckPointMessage] {
  def read(bytes: ByteVector): GetCompactFilterCheckPointMessage = ???

  def write(message: GetCompactFilterCheckPointMessage): ByteVector =
    message.filterType.bytes ++ message.stopHash.bytes
}
