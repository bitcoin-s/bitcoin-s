package org.bitcoins.core.p2p

import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.number.UInt32
import org.bitcoins.testkit.node.NodeTestUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.core.config.MainNet
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.crypto.CryptoUtil

import scala.util.Random
import scodec.bits.ByteVector

class NetworkHeaderTest extends BitcoinSUnitTest {

  "MessageHeader" must "must create a message header for a message" in {
    val messageHeader = NetworkHeader(TestNet3, NodeTestUtil.versionMessage)
    messageHeader.network must be(TestNet3)
    messageHeader.commandName must be(NodeTestUtil.versionMessage.commandName)
    messageHeader.payloadSize must be(
      UInt32(NodeTestUtil.versionMessage.bytes.size))
    messageHeader.checksum must be(
      CryptoUtil.doubleSHA256(NodeTestUtil.versionMessage.bytes).bytes.take(4))
  }

  it must "build the correct message header for a verack message" in {
    val messageHeader = NetworkHeader(TestNet3, VerAckMessage)
    messageHeader.network must be(TestNet3)
    messageHeader.commandName must be(VerAckMessage.commandName)
    messageHeader.payloadSize must be(UInt32.zero)
    BytesUtil.encodeHex(messageHeader.checksum) must be("5df6e0e2")
  }

  it must "throw on messages of bad length" in {
    intercept[IllegalArgumentException] {
      val commandName = Random.shuffle(NetworkPayload.commandNames).head
      NetworkHeader(MainNet,
                    commandName,
                    payloadSize = UInt32.one,
                    checksum = ByteVector.empty)
    }
  }

}
