package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.util.BitcoinSUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 8/31/16.
  */
class RawRejectMessageSerializerTest extends FlatSpec with MustMatchers {

  //https://bitcoin.org/en/developer-reference#reject
  val hex = "02" + "7478" + "12" + "15" + "6261642d74786e732d696e707574732d7370656e74" +
    "394715fcab51093be7bfca5a31005972947baf86a31017939575fb2354222821"

  "RawRejectMessageSerializer" must "read in a reject message example" in {
    val rejectMsg = RawRejectMessageSerializer.read(hex)
    rejectMsg.messageSize must be(CompactSizeUInt(UInt64(2)))
    rejectMsg.message must be("tx")
    rejectMsg.code must be(0x12.toChar)
    rejectMsg.reasonSize must be(CompactSizeUInt(UInt64(21)))
    rejectMsg.reason must be("bad-txns-inputs-spent")
    BitcoinSUtil.encodeHex(rejectMsg.extra) must be(
      "394715fcab51093be7bfca5a31005972947baf86a31017939575fb2354222821")
  }

  it must "read then write a reject message" in {
    val rejectMsg = RawRejectMessageSerializer.read(hex)
    rejectMsg.hex must be(hex)
  }
}
