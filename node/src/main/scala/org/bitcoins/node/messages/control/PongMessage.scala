package org.bitcoins.node.messages.control

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.util.Factory
import org.bitcoins.node.messages.PongMessage
import org.bitcoins.node.serializers.messages.control.RawPongMessageSerializer
import org.bitcoins.node.messages.PongMessage
import scodec.bits.ByteVector

/**
  * Created by chris on 7/5/16.
  */
object PongMessage extends Factory[PongMessage] {
  private case class PongMessageImpl(nonce: UInt64) extends PongMessage

  def fromBytes(bytes: ByteVector): PongMessage = {
    val pongMsg = RawPongMessageSerializer.read(bytes)
    PongMessageImpl(pongMsg.nonce)
  }

  def apply(nonce: UInt64): PongMessage = PongMessageImpl(nonce)
}
