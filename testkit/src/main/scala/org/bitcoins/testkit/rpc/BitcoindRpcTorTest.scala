package org.bitcoins.testkit.rpc

import org.bitcoins.testkit.node.{CachedBitcoinSAppConfigCachedTor}
import org.bitcoins.testkit.tor.CachedTor
import org.bitcoins.testkit.util.BitcoindRpcTest

trait BitcoindRpcTorTest
    extends BitcoindRpcTest
    with CachedBitcoinSAppConfigCachedTor
    with CachedTor {

  override def beforeAll(): Unit = {
    super[CachedTor].beforeAll()
    super[CachedBitcoinSAppConfigCachedTor].beforeAll()
    super[BitcoindRpcTest].beforeAll()
  }

  override def afterAll(): Unit = {
    super[BitcoindRpcTest].afterAll()
    super[CachedBitcoinSAppConfigCachedTor].afterAll()
    super[CachedTor].afterAll()
  }
}
