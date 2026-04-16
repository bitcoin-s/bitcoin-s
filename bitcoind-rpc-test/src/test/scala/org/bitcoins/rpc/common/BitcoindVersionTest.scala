package org.bitcoins.rpc.common

import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.testkit.util.BitcoindRpcTest

class BitcoindVersionTest extends BitcoindRpcTest {
  behavior of "BitcoindVersion"

  it should "return version 28" in {
    val version = BitcoindVersion.fromNetworkVersion(280100)
    assert(version.equals(BitcoindVersion.V28))
  }

  it should "return version 31" in {
    val version = BitcoindVersion.fromNetworkVersion(310000)
    assert(version.equals(BitcoindVersion.V31))
  }

}
