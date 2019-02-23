package org.bitcoins.node.messages.control

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.util.Factory
import org.bitcoins.node.messages.PingMessage
import org.bitcoins.node.serializers.messages.control.RawPingMessageSerializer
import org.bitcoins.node.messages.PingMessage
import scodec.bits.ByteVector

object PingMessage extends Factory[PingMessage] {
  private case class PingMessageImpl(nonce: UInt64) extends PingMessage
  override def fromBytes(bytes: ByteVector): PingMessage = {
    val pingMsg = RawPingMessageSerializer.read(bytes)
    PingMessageImpl(pingMsg.nonce)
  }

  def apply(nonce: UInt64): PingMessage = PingMessageImpl(nonce)
}
