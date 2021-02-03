package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.testkit.util.BitcoinSUnitTest

/** Created by chris on 6/1/16.
  */
class RawInventoryMessageSerializerTest extends BitcoinSUnitTest {

  //from bitcoin developer reference
  //https://bitcoin.org/en/developer-reference#inv
  val hex =
    "0201000000de55ffd709ac1f5dc509a0925d0b1fc442ca034f224732e429081da1b621f55a" +
      "0100000091d36d997037e08018262978766f24b8a055aaf1d872e94ae85e9817b2c68dc7"

  "RawInventoryMessageSerializer" must "read a InventoryMessage object from its hex serialization" in {
    val inventoryMessage = RawInventoryMessageSerializer.read(hex)
    inventoryMessage.inventoryCount must be(CompactSizeUInt(UInt64(2), 1))
    inventoryMessage.inventories.size must be(2)
  }

  it must "read and then write an inventory message and get its original hex" in {
    val inventoryMessage = RawInventoryMessageSerializer.read(hex)
    RawInventoryMessageSerializer.write(inventoryMessage).toHex must be(hex)
  }
}
