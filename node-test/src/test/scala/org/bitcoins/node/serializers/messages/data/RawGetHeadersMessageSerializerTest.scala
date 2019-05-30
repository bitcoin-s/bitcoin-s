package org.bitcoins.node.serializers.messages.data

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.node.versions.ProtocolVersion70002
import org.bitcoins.testkit.node.NodeTestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 6/29/16.
  */
class RawGetHeadersMessageSerializerTest extends FlatSpec with MustMatchers {
  val hex = NodeTestUtil.rawGetHeadersMsg

  "RawGetHeadersMessageSerializer" must "read a hex string representing a GetHeaderMessage" in {
    val getHeadersMessage = RawGetHeadersMessageSerializer.read(hex)
    getHeadersMessage.version must be(ProtocolVersion70002)
    getHeadersMessage.hashCount must be(CompactSizeUInt(UInt64(31), 1))
    getHeadersMessage.hashes.length must be(31)

    getHeadersMessage.hashStop must be(
      DoubleSha256Digest(
        "0000000000000000000000000000000000000000000000000000000000000000"))
  }

  it must "write a GetHeaderMessage" in {
    val getHeadersMessage = RawGetHeadersMessageSerializer.read(hex)
    RawGetHeadersMessageSerializer.write(getHeadersMessage).toHex must be(hex)
  }

}
