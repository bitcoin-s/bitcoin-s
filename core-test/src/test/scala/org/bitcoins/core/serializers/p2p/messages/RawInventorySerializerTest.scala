package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.p2p.TypeIdentifier.MsgTx
import org.bitcoins.crypto.{BytesUtil, DoubleSha256Digest}
import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 6/1/16.
  */
class RawInventorySerializerTest extends BitcoinSUnitTest {

  //from bitcoin developer reference example section
  //https://bitcoin.org/en/developer-reference#inv
  val hex =
    "01000000de55ffd709ac1f5dc509a0925d0b1fc442ca034f224732e429081da1b621f55a"
  "RawInventorySerializer" must "read a inventory object from its hex representation" in {
    val inventory = RawInventorySerializer.read(hex)
    inventory.typeIdentifier must be(MsgTx)
    inventory.hash must be
    (DoubleSha256Digest(
      BytesUtil.decodeHex(
        "de55ffd709ac1f5dc509a0925d0b1fc442ca034f224732e429081da1b621f55a")))
  }

  it must "write a inventory object to its serialized format" in {
    val inventory = RawInventorySerializer.read(hex)
    RawInventorySerializer.write(inventory).toHex must be(hex)
  }
}
