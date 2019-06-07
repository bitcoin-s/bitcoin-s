package org.bitcoins.core.p2p

import org.bitcoins.testkit.node.NodeTestUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.testkit.core.gen.p2p.P2PGenerator

class NetworkPayloadTest extends BitcoinSUnitTest {

  "NetworkMessage" must "create a payload object from it's network header and the payload bytes" in {
    val rawNetworkMessage = NodeTestUtil.rawNetworkMessage
    val header = NetworkHeader(rawNetworkMessage.take(48))
    logger.debug("Header: " + header)
    val payloadHex = rawNetworkMessage.slice(48, rawNetworkMessage.length)
    val payload = NetworkPayload(header, payloadHex)
    payload.isInstanceOf[VersionMessage] must be(true)
    payload.commandName must be(NetworkPayload.versionCommandName)
  }

  it must "parse messages based on its command name" in {
    forAll(P2PGenerator.message) { p2p =>
      val bytes = p2p.bytes
      val parser = NetworkPayload.commandNames(p2p.commandName)
      assert(parser(bytes) == p2p)
    }
  }
}
