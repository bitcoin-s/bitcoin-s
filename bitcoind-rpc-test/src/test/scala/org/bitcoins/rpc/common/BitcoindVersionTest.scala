package org.bitcoins.rpc.common

import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.testkit.util.BitcoindRpcTest

class BitcoindVersionTest extends BitcoindRpcTest {
  behavior of "BitcoindVersion"

  it should "return version 29" in {
    val version = BitcoindVersion.fromNetworkVersion(290100)
    assert(version.equals(BitcoindVersion.V29))
  }

  it should "return version 31" in {
    val version = BitcoindVersion.fromNetworkVersion(310000)
    assert(version.equals(BitcoindVersion.V31))
  }

}
