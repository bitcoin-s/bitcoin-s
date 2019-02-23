package org.bitcoins.node.serializers.messages.control

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.node.messages.PingMessage
import org.bitcoins.node.messages.control.PingMessage
import org.bitcoins.node.messages.PingMessage
import org.bitcoins.node.messages.control.PingMessage
import scodec.bits.ByteVector

/**
  * Created by chris on 6/29/16.
  * https://bitcoin.org/en/developer-reference#ping
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
