package org.bitcoins.rpc.common

import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.client.common.BitcoindVersion.{V19, V20, V21}
import org.bitcoins.testkit.util.BitcoindRpcTest

class BitcoindVersionTest extends BitcoindRpcTest {
  behavior of "BitcoindVersion"

  it should "return version 21" in {
    val version = BitcoindVersion.fromNetworkVersion(210100)
    assert(version.equals(V21))
  }
  it should "return version 20" in {
    val version = BitcoindVersion.fromNetworkVersion(200309)
    assert(version.equals(V20))
  }
  it should "return version 19" in {
    val version = BitcoindVersion.fromNetworkVersion(190100)
    assert(version.equals(V19))
  }

}
