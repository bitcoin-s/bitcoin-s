package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.node.P2PMessageTestUtil
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class NetworkMessageTest extends BitcoinSUnitTest {

  "NetworkMessage" must "be able to serialize then deserialize a message and get the original hex back" in {
    NetworkMessage(P2PMessageTestUtil.rawNetworkMessage).hex must be(
      P2PMessageTestUtil.rawNetworkMessage)
  }

  it must "serialize and deserialize a version message example from the bitcoin wiki" in {
    val hex = {
      //taken from here with slight modifications
      //https://en.bitcoin.it/wiki/Protocol_documentation#Message_structure
      //this example uses an old protocol version WITHOUT the relay flag on the version message
      //since we only support protocol version > 7, i added it manually
      //this means the payload size is bumped by 1 byte in the NetworkHeader from 100 -> 101
      //and a relay byte "00" is appended to the end of the payload
      "F9BEB4D976657273696F6E000000000065000000358d4932" +
        "62EA0000010000000000000011B2D05000000000010000000000000000000000000000000000FFFF000000000000010000000000000000000000000000000000FFFF0000000000003B2EB35D8CE617650F2F5361746F7368693A302E372E322FC03E0300" +
        "00"
    }.toLowerCase
    val networkMsg = NetworkMessage.fromHex(hex)
    networkMsg.hex must be(hex)
  }
}
