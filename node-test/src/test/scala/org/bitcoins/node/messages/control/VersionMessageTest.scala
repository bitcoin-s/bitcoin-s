package org.bitcoins.node.messages.control

import java.net.InetAddress

import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.core.config.MainNet
import org.bitcoins.core.number.{Int32, UInt64}
import org.joda.time.DateTime

class VersionMessageTest extends BitcoinSUnitTest {

  "VersionMessage" must "create a new version message to be sent to another node on the network" in {
    val versionMessage = VersionMessage(MainNet, InetAddress.getLocalHost)
    versionMessage.addressReceiveServices must be(UnnamedService)
    versionMessage.addressReceiveIpAddress must be(InetAddress.getLocalHost)
    versionMessage.addressReceivePort must be(MainNet.port)

    versionMessage.addressTransServices must be(NodeNetwork)
    versionMessage.addressTransIpAddress must be(InetAddress.getLocalHost)
    versionMessage.addressTransPort must be(MainNet.port)

    versionMessage.nonce must be(UInt64.zero)
    versionMessage.startHeight must be(Int32.zero)
    versionMessage.timestamp.toLong must be(DateTime.now.getMillis +- 1000)
  }
}
