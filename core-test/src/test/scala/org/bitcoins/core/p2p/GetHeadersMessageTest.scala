package org.bitcoins.core.p2p

import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.testkitcore.Implicits._
import org.bitcoins.testkitcore.gen.CryptoGenerators
import org.bitcoins.testkitcore.gen.p2p.DataMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class GetHeadersMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.getHeaderMessages) { headerMsg =>
      assert(GetHeadersMessage(headerMsg.hex) == headerMsg)
    }
  }

  it must "be constructable from just hashes" in {
    forAll(DataMessageGenerator.getHeaderDefaultProtocolMessage) { getHeader =>
      assert(
        GetHeadersMessage(getHeader.hashes, getHeader.hashStop) == getHeader
      )
    }
  }

  it must "be constructable without a stop" in {
    def getHash(): DoubleSha256Digest =
      CryptoGenerators.doubleSha256Digest.sampleSome

    val msg = GetHeadersMessage(Vector.fill(10)(getHash()))
    assert(msg.hashStop == DoubleSha256Digest.empty)

    val hash = getHash()
    val otherMsg = GetHeadersMessage(hash)
    assert(otherMsg == GetHeadersMessage(Vector(hash)))
  }

  it must "have a meaningful toString" in {
    forAll(DataMessageGenerator.getHeaderMessages) { message =>
      assert(message.toString().length() < 300)
    }
  }
}
