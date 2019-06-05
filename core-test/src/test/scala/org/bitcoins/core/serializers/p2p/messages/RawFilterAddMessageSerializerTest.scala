package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.util.BitcoinSUtil
import org.scalatest.{FlatSpec, MustMatchers}

class RawFilterAddMessageSerializerTest extends FlatSpec with MustMatchers {

  //https://bitcoin.org/en/developer-reference#filteradd
  val hex = "20" + "fdacf9b3eb077412e7a968d2e4f11b9a9dee312d666187ed77ee7d26af16cb0b"
  "RawFilterAddMessageSerializer" must "deserialize a message in the bitcoin developer reference" in {
    val filterAddMsg = RawFilterAddMessageSerializer.read(hex)
    filterAddMsg.elementSize.hex must be("20")
    BitcoinSUtil.encodeHex(filterAddMsg.element) must be(
      "fdacf9b3eb077412e7a968d2e4f11b9a9dee312d666187ed77ee7d26af16cb0b")
  }

  it must "serialize a filter add message" in {
    val filterAddMsg = RawFilterAddMessageSerializer.read(hex)
    filterAddMsg.hex must be(hex)
  }
}
