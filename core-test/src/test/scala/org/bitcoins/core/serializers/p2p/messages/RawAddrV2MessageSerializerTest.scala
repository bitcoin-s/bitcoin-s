package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.number.{UInt16, UInt32, UInt64}
import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.testkitcore.gen.NumberGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalacheck.Gen
import scodec.bits.{ByteVector, HexStringSyntax}

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
      addrBytes <-
        NumberGenerator.bytevector(AddrV2Message.CJDNS_ADDR_LENGTH - 1)
      port <- NumberGenerator.uInt16
    } yield CJDNSAddrV2Message(time,
                               services,
                               ByteVector.fromByte(0xfc.toByte) ++ addrBytes,
                               port)
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

  it must "parse an IPv4AddrV2Message" in {
    val msg = IPv4AddrV2Message(UInt32(4523),
                                CompactSizeUInt(UInt64(53453453L)),
                                InetAddress.getByAddress(hex"00000000".toArray),
                                UInt16(8333))

    assert("000011abfe8da22f030100000000208d" == msg.hex)
  }

  "IPv6AddrV2Message" must "have serialization symmetry" in {
    forAll(ipv6AddrV2MessageGen) { msg =>
      assert(msg.bytes == AddrV2Message(msg.bytes).bytes)
    }
  }

  it must "parse an IPv6AddrV2Message" in {
    val msg = IPv6AddrV2Message(
      UInt32(4523),
      CompactSizeUInt(UInt64(53453453L)),
      InetAddress.getByAddress(hex"00000000000000000000000000000000".toArray),
      UInt16(8333))

    assert(
      "000011abfe8da22f030200000000000000000000000000000000208d" == msg.hex)
  }

  "TorV2AddrV2Message" must "have serialization symmetry" in {
    forAll(torV2AddrV2MessageGen) { msg =>
      assert(msg.bytes == AddrV2Message(msg.bytes).bytes)
    }
  }

  it must "parse a TorV2AddrV2Message" in {
    val msg = TorV2AddrV2Message(UInt32(4523),
                                 CompactSizeUInt(UInt64(53453453L)),
                                 hex"00000000000000000000",
                                 UInt16(8333))

    assert("000011abfe8da22f030300000000000000000000208d" == msg.hex)
  }

  "TorV3AddrV2Message" must "have serialization symmetry" in {
    forAll(torV3AddrV2MessageGen) { msg =>
      assert(msg.bytes == AddrV2Message(msg.bytes).bytes)
    }
  }

  it must "parse a TorV3AddrV2Message" in {
    val msg = TorV3AddrV2Message(
      UInt32(4523),
      CompactSizeUInt(UInt64(53453453L)),
      hex"0000000000000000000000000000000000000000000000000000000000000000",
      UInt16(8333))

    assert(
      "000011abfe8da22f03040000000000000000000000000000000000000000000000000000000000000000208d" == msg.hex)
  }

  "I2PAddrV2Message" must "have serialization symmetry" in {
    forAll(i2pAddrV2MessageGen) { msg =>
      assert(msg.bytes == AddrV2Message(msg.bytes).bytes)
    }
  }

  it must "parse an I2PAddrV2Message" in {
    val msg = I2PAddrV2Message(
      UInt32(4523),
      CompactSizeUInt(UInt64(53453453L)),
      hex"0000000000000000000000000000000000000000000000000000000000000000",
      UInt16(8333))

    assert(
      "000011abfe8da22f03050000000000000000000000000000000000000000000000000000000000000000208d" == msg.hex)
  }

  "CJDNSAddrV2Message" must "have serialization symmetry" in {
    forAll(cjdnsAddrV2MessageGen) { msg =>
      assert(msg.bytes == AddrV2Message(msg.bytes).bytes)
    }
  }

  it must "parse a CJDNSAddrV2Message" in {
    val msg = CJDNSAddrV2Message(UInt32(4523),
                                 CompactSizeUInt(UInt64(53453453L)),
                                 hex"fc000000000000000000000000000000",
                                 UInt16(8333))

    assert(
      "000011abfe8da22f0306fc000000000000000000000000000000208d" == msg.hex)
  }

  "UnknownNetworkAddrV2Message" must "have serialization symmetry" in {
    forAll(unknownAddrV2MessageGen) { msg =>
      assert(msg.bytes == AddrV2Message(msg.bytes).bytes)
    }
  }

  it must "parse a UnknownNetworkAddrV2Message" in {
    val msg = UnknownNetworkAddrV2Message(UInt32(4523),
                                          CompactSizeUInt(UInt64(53453453L)),
                                          0x07,
                                          hex"00000000000000000000000000000000",
                                          UInt16(8333))

    assert(
      "000011abfe8da22f030700000000000000000000000000000000208d" == msg.hex)
  }
}
