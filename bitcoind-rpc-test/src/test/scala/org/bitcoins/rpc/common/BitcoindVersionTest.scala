package org.bitcoins.rpc.common

import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.client.common.BitcoindVersion.V21
import org.bitcoins.testkit.util.BitcoindRpcTest

class BitcoindVersionTest extends BitcoindRpcTest {
  behavior of "BitcoindVersion"

  it should "return version 21" in {
    val version = BitcoindVersion.fromNetworkVersion(210100)
    assert(version.equals(V21))
  }

}
