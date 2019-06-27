package org.bitcoins.core.p2p

import org.bitcoins.testkit.core.gen.p2p.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.Implicits._

class GetHeadersMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.getHeaderMessages) { headerMsg =>
      assert(GetHeadersMessage(headerMsg.hex) == headerMsg)
    }
  }

  it must "be constructable from just hashes" in {
    forAll(DataMessageGenerator.getHeaderDefaultProtocolMessage) { getHeader =>
      assert(
        GetHeadersMessage(getHeader.hashes, getHeader.hashStop) == getHeader)
    }
  }

  it must "be constructable without a stop" in {
    def getHash(): DoubleSha256Digest =
      CryptoGenerators.doubleSha256Digest.sampleSome

    val msg = GetHeadersMessage(List.fill(10)(getHash()))
    assert(msg.hashStop == DoubleSha256Digest.empty)

    val hash = getHash()
    val otherMsg = GetHeadersMessage(hash)
    assert(otherMsg == GetHeadersMessage(Vector(hash)))
  }
}
