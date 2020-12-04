package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.p2p._
import org.bitcoins.testkit.core.gen.NumberGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalacheck.Gen

import java.net.InetAddress

class RawAddrV2MessageSerializerTest extends BitcoinSUnitTest {

  def ipv4AddrV2MessageGen: Gen[IPv4AddrV2Message] = {
    for {
      time <- NumberGenerator.uInt32s
      services <- NumberGenerator.compactSizeUInts
      addrBytes <- NumberGenerator.bytevector(AddrV2Message.IPV4_ADDR_LENGTH)
      port <- NumberGenerator.uInt16
    } yield IPv4AddrV2Message(time,
                              services,
                              InetAddress.getByAddress(addrBytes.toArray),
                              port)
  }

  def ipv6AddrV2MessageGen: Gen[IPv6AddrV2Message] = {
    for {
      time <- NumberGenerator.uInt32s
      services <- NumberGenerator.compactSizeUInts
      addrBytes <- NumberGenerator.bytevector(AddrV2Message.IPV6_ADDR_LENGTH)
      port <- NumberGenerator.uInt16
    } yield IPv6AddrV2Message(time,
                              services,
                              InetAddress.getByAddress(addrBytes.toArray),
                              port)
  }

  def torV2AddrV2MessageGen: Gen[TorV2AddrV2Message] = {
    for {
      time <- NumberGenerator.uInt32s
      services <- NumberGenerator.compactSizeUInts
      addrBytes <- NumberGenerator.bytevector(AddrV2Message.TOR_V2_ADDR_LENGTH)
      port <- NumberGenerator.uInt16
    } yield TorV2AddrV2Message(time, services, addrBytes, port)
  }

  def torV3AddrV2MessageGen: Gen[TorV3AddrV2Message] = {
    for {
      time <- NumberGenerator.uInt32s
      services <- NumberGenerator.compactSizeUInts
      addrBytes <- NumberGenerator.bytevector(AddrV2Message.TOR_V3_ADDR_LENGTH)
      port <- NumberGenerator.uInt16
    } yield TorV3AddrV2Message(time, services, addrBytes, port)
  }

  def i2pAddrV2MessageGen: Gen[I2PAddrV2Message] = {
    for {
      time <- NumberGenerator.uInt32s
      services <- NumberGenerator.compactSizeUInts
      addrBytes <- NumberGenerator.bytevector(AddrV2Message.I2P_ADDR_LENGTH)
      port <- NumberGenerator.uInt16
    } yield I2PAddrV2Message(time, services, addrBytes, port)
  }

  def cjdnsAddrV2MessageGen: Gen[CJDNSAddrV2Message] = {
    for {
      time <- NumberGenerator.uInt32s
      services <- NumberGenerator.compactSizeUInts
      addrBytes <- NumberGenerator.bytevector(AddrV2Message.CJDNS_ADDR_LENGTH)
      port <- NumberGenerator.uInt16
    } yield CJDNSAddrV2Message(time, services, addrBytes, port)
  }

  def unknownAddrV2MessageGen: Gen[UnknownNetworkAddrV2Message] = {
    for {
      time <- NumberGenerator.uInt32s
      services <- NumberGenerator.compactSizeUInts
      networkId <- NumberGenerator.byte.suchThat(byte =>
        !AddrV2Message.knownNetworkBytes.contains(byte))
      addrBytes <- NumberGenerator.bytevector
      port <- NumberGenerator.uInt16
    } yield UnknownNetworkAddrV2Message(time,
                                        services,
                                        networkId,
                                        addrBytes,
                                        port)
  }

  "IPv4AddrV2Message" must "have serialization symmetry" in {
    forAll(ipv4AddrV2MessageGen) { msg =>
      assert(msg.bytes == AddrV2Message(msg.bytes).bytes)
    }
  }

  "IPv6AddrV2Message" must "have serialization symmetry" in {
    forAll(ipv6AddrV2MessageGen) { msg =>
      assert(msg.bytes == AddrV2Message(msg.bytes).bytes)
    }
  }

  "TorV2AddrV2Message" must "have serialization symmetry" in {
    forAll(torV2AddrV2MessageGen) { msg =>
      assert(msg.bytes == AddrV2Message(msg.bytes).bytes)
    }
  }

  "TorV3AddrV2Message" must "have serialization symmetry" in {
    forAll(torV3AddrV2MessageGen) { msg =>
      assert(msg.bytes == AddrV2Message(msg.bytes).bytes)
    }
  }

  "I2PAddrV2Message" must "have serialization symmetry" in {
    forAll(i2pAddrV2MessageGen) { msg =>
      assert(msg.bytes == AddrV2Message(msg.bytes).bytes)
    }
  }

  "CJDNSAddrV2Message" must "have serialization symmetry" in {
    forAll(cjdnsAddrV2MessageGen) { msg =>
      assert(msg.bytes == AddrV2Message(msg.bytes).bytes)
    }
  }

  "UnknownNetworkAddrV2Message" must "have serialization symmetry" in {
    forAll(unknownAddrV2MessageGen) { msg =>
      assert(msg.bytes == AddrV2Message(msg.bytes).bytes)
    }
  }
}
