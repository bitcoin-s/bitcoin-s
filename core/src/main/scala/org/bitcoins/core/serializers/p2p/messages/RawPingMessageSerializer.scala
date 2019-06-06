package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.p2p.PingMessage
import org.bitcoins.core.p2p.PingMessage
import scodec.bits.ByteVector

/**
  * @see https://bitcoin.org/en/developer-reference#ping
  */
trait RawPingMessageSerializer extends RawBitcoinSerializer[PingMessage] {

  override def read(bytes: ByteVector): PingMessage = {
    val nonce = UInt64(bytes.take(8))
    PingMessage(nonce)
  }

  override def write(pingMessage: PingMessage): ByteVector =
    pingMessage.nonce.bytes
}

object RawPingMessageSerializer extends RawPingMessageSerializer
