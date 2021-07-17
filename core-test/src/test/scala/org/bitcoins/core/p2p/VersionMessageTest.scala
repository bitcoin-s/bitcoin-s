package org.bitcoins.core.p2p

import org.bitcoins.core.config.MainNet
import org.bitcoins.core.number.{Int32, UInt64}
import org.bitcoins.testkitcore.gen.p2p.ControlMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits._

import java.time.Instant

class VersionMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.versionMessage) { versionMessage =>
      assert(VersionMessage(versionMessage.hex) == versionMessage)
    }
  }

  it must "have a meaningful toString message" in {
    forAll(ControlMessageGenerator.versionMessage) { version =>
      assert(version.toString().length < 400 + version.userAgent.length())
    }
  }

  "VersionMessage" must "create a new version message to be sent to another node on the network" in {
    val ipArr = Array(173.toByte, 31.toByte, 39.toByte, 168.toByte)
    val inet = InetAddress(ipArr)

    val versionMessage = VersionMessage(MainNet, inet, inet, relay = false)
    assert(versionMessage.addressReceiveServices.nodeNone)
    versionMessage.addressReceiveIpAddress must be(inet)
    versionMessage.addressReceivePort must be(MainNet.port)

    assert(versionMessage.addressTransServices.nodeNetwork)
    versionMessage.addressTransIpAddress must be(inet)
    versionMessage.addressTransPort must be(MainNet.port)

    versionMessage.nonce must be(UInt64.zero)
    versionMessage.startHeight must be(Int32.zero)
    versionMessage.timestamp.toLong must be(
      Instant.now().getEpochSecond +- 1000)
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
