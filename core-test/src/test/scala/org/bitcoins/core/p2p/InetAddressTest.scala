package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits._

class InetAddressTest extends BitcoinSUnitTest {
  behavior of "InetAddress"

  it must "correctly read an IPv4 address" in {
    val ipv6Bytes = hex"00000000000000000000ffff48c4a809"
    val ipv4Bytes = hex"48c4a809"

    val inetA = InetAddress(ipv6Bytes)
    assert(inetA.ipv4Bytes == ipv4Bytes)
    val inetB = InetAddress(ipv4Bytes)
    assert(inetB.bytes == ipv6Bytes)
    assert(inetA.getAddress sameElements ipv6Bytes.toArray)
  }

  it must "correctly read an IPv6 address" in {
    val bytes = hex"20010db800002f3b02aa00fffe289c5a"

    val inet = InetAddress(bytes)
    assert(inet.bytes == bytes)
    assert(inet.getAddress sameElements bytes.toArray)
    assertThrows[IllegalArgumentException](inet.ipv4Bytes)
  }
}
