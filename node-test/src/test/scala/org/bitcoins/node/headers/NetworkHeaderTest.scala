package org.bitcoins.node.headers

import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.util.{BitcoinSUtil, CryptoUtil}
import org.bitcoins.node.messages.VerAckMessage
import org.bitcoins.testkit.node.NodeTestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 6/10/16.
  */
class NetworkHeaderTest extends FlatSpec with MustMatchers {

  "MessageHeader" must "must create a message header for a message" in {
    val messageHeader = NetworkHeader(TestNet3, NodeTestUtil.versionMessage)
    messageHeader.network must be(TestNet3.magicBytes)
    messageHeader.commandName must be(NodeTestUtil.versionMessage.commandName)
    messageHeader.payloadSize must be(
      UInt32(NodeTestUtil.versionMessage.bytes.size))
    messageHeader.checksum must be(
      CryptoUtil.doubleSHA256(NodeTestUtil.versionMessage.bytes).bytes.take(4))
  }

  it must "build the correct message header for a verack message" in {
    val messageHeader = NetworkHeader(TestNet3, VerAckMessage)
    messageHeader.network must be(TestNet3.magicBytes)
    messageHeader.commandName must be(VerAckMessage.commandName)
    messageHeader.payloadSize must be(UInt32.zero)
    BitcoinSUtil.encodeHex(messageHeader.checksum) must be("5df6e0e2")
  }

}
