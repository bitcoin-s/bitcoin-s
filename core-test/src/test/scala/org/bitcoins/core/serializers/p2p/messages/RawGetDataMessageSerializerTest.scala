package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.p2p.Inventory
import org.bitcoins.core.p2p.TypeIdentifier.MsgTx
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 7/8/16.
  */
class RawGetDataMessageSerializerTest extends BitcoinSUnitTest {

  //from bitcoin developer reference
  //a getdata message is essentially an inv message
  //https://bitcoin.org/en/developer-reference#inv
  val hex =
    "02" + "01000000" + "de55ffd709ac1f5dc509a0925d0b1fc442ca034f224732e429081da1b621f55a" +
      "01000000" + "91d36d997037e08018262978766f24b8a055aaf1d872e94ae85e9817b2c68dc7"

  "RawGetDataMessageSerializer" must "read in a data message" in {
    val dataMsg = RawGetDataMessageSerializer.read(hex)
    dataMsg.inventoryCount must be(CompactSizeUInt(UInt64(2)))
    dataMsg.inventories.head must be(
      Inventory(
        MsgTx,
        DoubleSha256Digest(
          "de55ffd709ac1f5dc509a0925d0b1fc442ca034f224732e429081da1b621f55a")))
    dataMsg.inventories(1) must be(
      Inventory(
        MsgTx,
        DoubleSha256Digest(
          "91d36d997037e08018262978766f24b8a055aaf1d872e94ae85e9817b2c68dc7")))

  }

  it must "write a GetDataMessage back to the original hex" in {
    val dataMsg = RawGetDataMessageSerializer.read(hex)
    RawGetDataMessageSerializer.write(dataMsg)
  }
}
