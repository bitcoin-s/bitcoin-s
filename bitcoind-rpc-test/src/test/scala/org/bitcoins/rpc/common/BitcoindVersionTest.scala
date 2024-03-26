package org.bitcoins.rpc.common

import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.testkit.util.BitcoindRpcTest

class BitcoindVersionTest extends BitcoindRpcTest {
  behavior of "BitcoindVersion"

  it should "return version 24" in {
    val version = BitcoindVersion.fromNetworkVersion(240100)
    assert(version.equals(BitcoindVersion.V24))
  }

}
