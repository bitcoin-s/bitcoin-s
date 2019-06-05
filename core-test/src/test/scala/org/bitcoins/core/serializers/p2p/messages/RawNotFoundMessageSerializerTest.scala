package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.CompactSizeUInt
import org.scalatest.{FlatSpec, MustMatchers}

class RawNotFoundMessageSerializerTest extends FlatSpec with MustMatchers {

  //according to the developer reference, the format for inventory messages and
  //not found messages are the same
  //from bitcoin developer reference
  //https://bitcoin.org/en/developer-reference#inv
  val hex = "0201000000de55ffd709ac1f5dc509a0925d0b1fc442ca034f224732e429081da1b621f55a" +
    "0100000091d36d997037e08018262978766f24b8a055aaf1d872e94ae85e9817b2c68dc7"

  "RawNotFoundMessageSerializer" must "read a not found message from its hex representation" in {
    val notFoundMessage = RawNotFoundMessageSerializer.read(hex)
    notFoundMessage.inventoryCount must be(CompactSizeUInt(UInt64(2), 1))
    notFoundMessage.inventories.size must be(2)
  }

  it must "write a not found message and get its original hex representation back" in {
    val notFoundMessage = RawNotFoundMessageSerializer.read(hex)
    RawNotFoundMessageSerializer.write(notFoundMessage).toHex must be(hex)
  }

}
