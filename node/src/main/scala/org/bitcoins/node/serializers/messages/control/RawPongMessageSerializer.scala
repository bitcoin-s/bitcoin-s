package org.bitcoins.node.serializers.messages.control

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.node.messages.PongMessage
import org.bitcoins.node.messages.control.PongMessage
import org.bitcoins.node.messages.PongMessage
import org.bitcoins.node.messages.control.PongMessage
import scodec.bits.ByteVector

/**
  * Created by chris on 7/5/16.
  */
trait RawPongMessageSerializer extends RawBitcoinSerializer[PongMessage] {

  override def read(bytes: ByteVector): PongMessage = {
    PongMessage(UInt64(bytes.take(8)))
  }

  override def write(pongMessage: PongMessage): ByteVector = {
    pongMessage.nonce.bytes
  }
}

object RawPongMessageSerializer extends RawPongMessageSerializer
