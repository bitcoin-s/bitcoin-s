package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits._
import org.bitcoins.core.p2p.CompactFilterHeadersMessage
import org.bitcoins.core.p2p.NetworkHeader
import org.bitcoins.core.gcs.FilterType

class RawCompactFilterHeadersMessageSerializerTest extends BitcoinSUnitTest {
  it must "parse a message" in {
    // cribbed from a P2P log dump with Bitcoin-S node
    val bytes =
      hex"00226f1acd30ec19b541dba2478b673a142283bb39ccddef75015e3caf0ec89f480000000000000000000000000000000000000000000000000000000000000000031f30de30fabb7892d15eb985cc5d6c34c54a11b7e4c51f3da498f16255a27bb157965194aaa7ad3890c977d1b3c738d0a43a357ec645df28dc5c21876fb529c487eb3f35daf3b6adba13b40c2f0d0e99dee59b624b0e09d870894e1a0d6d3bb0"
    val message = CompactFilterHeadersMessage.fromBytes(bytes)

    assert(message.filterType == FilterType.Basic)
    assert(message.filterHashes.length == 3)
  }

  // TODO, create generator
  it must "have serialization symmetry" ignore ???
}
