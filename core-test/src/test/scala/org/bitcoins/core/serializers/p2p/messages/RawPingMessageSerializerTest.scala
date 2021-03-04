package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.p2p.PingMessage
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 6/29/16.
  */
class RawPingMessageSerializerTest extends BitcoinSUnitTest {

  "RawPingMessageSerializer" must "read and write a uint64 representing the ping" in {
    val hex = "0094102111e2af4d"
    RawPingMessageSerializer.read(hex) must be(PingMessage(UInt64(hex)))
  }

  it must "write a ping message" in {
    val pingMessage = PingMessage(UInt64.zero)
    pingMessage.hex must be("0000000000000000")
  }
}
