package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.p2p.PongMessage
import org.bitcoins.core.p2p.PongMessage
import scodec.bits.ByteVector

trait RawPongMessageSerializer extends RawBitcoinSerializer[PongMessage] {

  override def read(bytes: ByteVector): PongMessage = {
    PongMessage(UInt64(bytes.take(8)))
  }

  override def write(pongMessage: PongMessage): ByteVector = {
    pongMessage.nonce.bytes
  }
}

object RawPongMessageSerializer extends RawPongMessageSerializer
