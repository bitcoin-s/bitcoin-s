package org.bitcoins.node.serializers.messages.data

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.util.BitcoinSUtil
import org.bitcoins.node.messages.TypeIdentifier.MsgTx
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 6/1/16.
  */
class RawInventorySerializerTest extends FlatSpec with MustMatchers {

  //from bitcoin developer reference example section
  //https://bitcoin.org/en/developer-reference#inv
  val hex =
    "01000000de55ffd709ac1f5dc509a0925d0b1fc442ca034f224732e429081da1b621f55a"
  "RawInventorySerializer" must "read a inventory object from its hex representation" in {
    val inventory = RawInventorySerializer.read(hex)
    inventory.typeIdentifier must be(MsgTx)
    inventory.hash must be
    (DoubleSha256Digest(
      BitcoinSUtil.decodeHex(
        "de55ffd709ac1f5dc509a0925d0b1fc442ca034f224732e429081da1b621f55a")))
  }

  it must "write a inventory object to its serialized format" in {
    val inventory = RawInventorySerializer.read(hex)
    RawInventorySerializer.write(inventory).toHex must be(hex)
  }
}
