package org.bitcoins.node.messages

import org.bitcoins.node.headers.NetworkHeader
import org.bitcoins.testkit.node.NodeTestUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest

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
}
