package org.bitcoins.core.p2p

import org.bitcoins.core.config.TestNet3
import org.bitcoins.testkitcore.gen.p2p.P2PGenerator
import org.bitcoins.testkitcore.node.P2PMessageTestUtil
import org.bitcoins.testkitcore.util.BitcoinSJvmTest

class NetworkPayloadTest extends BitcoinSJvmTest {

  "NetworkMessage" must "create a payload object from it's network header and the payload bytes" in {
    val rawNetworkMessage = P2PMessageTestUtil.rawNetworkMessage
    val header = NetworkHeader(rawNetworkMessage.take(48))
    logger.debug("Header: " + header)
    val payloadHex = rawNetworkMessage.slice(48, rawNetworkMessage.length)
    val payload = NetworkPayload(header, payloadHex)
    payload.isInstanceOf[VersionMessage] must be(true)
    payload.commandName must be(NetworkPayload.versionCommandName)
    val testVersionMessage = VersionMessage("173.31.39.168", TestNet3)
    payload.asInstanceOf[VersionMessage].addressReceiveIpAddress must be(
      testVersionMessage.addressReceiveIpAddress)
    payload.asInstanceOf[VersionMessage].addressReceivePort must be(
      testVersionMessage.addressReceivePort)
  }

  // this tests has a bunch of messages to choose between, so we set a high config value
  implicit override val generatorDrivenConfig = customGenDrivenConfig(100)
  it must "parse messages based on its command name" in {
    forAllParallel(P2PGenerator.message) { p2p =>
      val bytes = p2p.bytes
      val parser = NetworkPayload.readers(p2p.commandName)
      assert(parser(bytes) == p2p)
    }
  }
}
