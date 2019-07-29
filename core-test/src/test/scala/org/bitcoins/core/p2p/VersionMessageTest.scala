package org.bitcoins.core.p2p

import java.net.InetAddress
import java.time.Instant

import org.bitcoins.core.config.MainNet
import org.bitcoins.core.number.{Int32, UInt64}
import org.bitcoins.testkit.core.gen.p2p.ControlMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits._

class VersionMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.versionMessage) { versionMessage =>
      assert(VersionMessage(versionMessage.hex) == versionMessage)
    }
  }

  "VersionMessage" must "create a new version message to be sent to another node on the network" in {
    val versionMessage = VersionMessage(MainNet, InetAddress.getLocalHost)
    assert(versionMessage.addressReceiveServices.nodeNone)
    versionMessage.addressReceiveIpAddress must be(InetAddress.getLocalHost)
    versionMessage.addressReceivePort must be(MainNet.port)

    assert(versionMessage.addressTransServices.nodeNetwork)
    versionMessage.addressTransIpAddress must be(InetAddress.getLocalHost)
    versionMessage.addressTransPort must be(MainNet.port)

    versionMessage.nonce must be(UInt64.zero)
    versionMessage.startHeight must be(Int32.zero)
    versionMessage.timestamp.toLong must be(Instant.now().toEpochMilli +- 1000)
  }

  it must "correctly deduce service flags" in {
    // extracted from log dump of local bitcoind running 0.17.0.1
    val msgBytes =
      hex"7f1101000d040000000000004ea1035d0000000000000000000000000000000000000000000000000000000000000d04000000000000000000000000000000000000000000000000fa562b93b3113e02122f5361746f7368693a302e31372e302e312f6800000001"
    val versionMessage = VersionMessage.fromBytes(msgBytes)

    assert(versionMessage.services.nodeNetwork)
    assert(!versionMessage.services.nodeGetUtxo)
    assert(versionMessage.services.nodeBloom)
    assert(versionMessage.services.nodeWitness)
    assert(!versionMessage.services.nodeXthin)
    assert(versionMessage.services.nodeNetworkLimited)
    assert(!versionMessage.services.nodeNone)
  }
}
