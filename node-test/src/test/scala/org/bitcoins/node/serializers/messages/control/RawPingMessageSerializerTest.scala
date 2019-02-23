package org.bitcoins.node.serializers.messages.control

import org.bitcoins.core.number.UInt64
import org.bitcoins.node.messages.control.PingMessage
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 6/29/16.
  */
class RawPingMessageSerializerTest extends FlatSpec with MustMatchers {

  "RawPingMessageSerializer" must "read and write a uint64 representing the ping" in {
    val hex = "0094102111e2af4d"
    RawPingMessageSerializer.read(hex) must be(PingMessage(UInt64(hex)))
  }

  it must "write a ping message" in {
    val pingMessage = PingMessage(UInt64.zero)
    pingMessage.hex must be("0000000000000000")
  }
}
