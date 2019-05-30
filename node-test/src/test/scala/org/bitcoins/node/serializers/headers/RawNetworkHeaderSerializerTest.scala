package org.bitcoins.node.serializers.headers

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.util.BitcoinSUtil
import org.bitcoins.node.messages.NetworkPayload
import org.bitcoins.testkit.node.NodeTestUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest

class RawNetworkHeaderSerializerTest extends BitcoinSUnitTest {
  val hex = "f9beb4d976657261636b000000000000000000005df6e0e2"
  "RawMessageHeaderSerializer" must "read hex string into a message header" in {
    //this example is from this section in the bitcoin developer reference
    //https://bitcoin.org/en/developer-reference#message-headers

    val messageHeader = RawNetworkHeaderSerializer.read(hex)
    //this is the mainnet id
    BitcoinSUtil.encodeHex(messageHeader.network) must be("f9beb4d9")

    messageHeader.commandName must be("verack")

    messageHeader.payloadSize must be(UInt32.zero)

    BitcoinSUtil.encodeHex(messageHeader.checksum) must be("5df6e0e2")
  }

  it must "write an object that was just read and get the original input" in {
    val messageHeader = RawNetworkHeaderSerializer.read(hex)
    messageHeader.hex must be(hex)
  }

  it must "read a network header from a node on the network" in {
    val hex = NodeTestUtil.rawNetworkMessage.take(48)
    val header = RawNetworkHeaderSerializer.read(hex)
    BitcoinSUtil.encodeHex(header.network) must be("0B110907".toLowerCase)
    header.commandName.size must be(NetworkPayload.versionCommandName.size)
    header.commandName must be(NetworkPayload.versionCommandName)
    header.payloadSize must be(UInt32(102))
    BitcoinSUtil.encodeHex(header.checksum) must be("2f6743da")

  }

}
