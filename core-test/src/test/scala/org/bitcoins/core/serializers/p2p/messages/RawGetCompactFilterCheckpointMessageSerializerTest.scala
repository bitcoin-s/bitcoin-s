package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.p2p.GetCompactFilterCheckPointMessage
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits._

class RawGetCompactFilterCheckpointMessageSerializerTest
    extends BitcoinSUnitTest {

  it must "parse a message" in {
    // cribbed from a P2P log dump with Bitcoin-S node
    val bytes = hex"00" ++ //type
      hex"06226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f" // stop hash

    val message = GetCompactFilterCheckPointMessage.fromBytes(bytes)

    assert(message.filterType == FilterType.Basic)
    assert(
      message.stopHash == DoubleSha256Digest.fromHex(
        "06226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f"))
  }

  it must "have serialization symmetry" in {

    val bytes = hex"00" ++ //type
      hex"06226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f" // stop hash

    val message = GetCompactFilterCheckPointMessage.fromBytes(bytes)

    assert(bytes == message.bytes)

    val anotherMessage = GetCompactFilterCheckPointMessage(
      DoubleSha256Digest.fromHex(
        "8000000000000000000000000000000000000000000000000000000000000001"))

    val anotherBytes = anotherMessage.bytes

    assert(
      anotherMessage == GetCompactFilterCheckPointMessage.fromBytes(
        anotherBytes))
  }

}
