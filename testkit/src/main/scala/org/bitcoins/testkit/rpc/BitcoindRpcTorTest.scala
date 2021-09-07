package org.bitcoins.testkit.rpc

import org.bitcoins.testkit.node.{CachedBitcoinSAppConfigCachedTor}
import org.bitcoins.testkit.util.BitcoindRpcTest

trait BitcoindRpcTorTest
    extends BitcoindRpcTest
    with CachedBitcoinSAppConfigCachedTor {

  override def beforeAll(): Unit = {
    super[CachedBitcoinSAppConfigCachedTor].beforeAll()
    super[BitcoindRpcTest].beforeAll()
  }

  override def afterAll(): Unit = {
    super[BitcoindRpcTest].afterAll()
    super[CachedBitcoinSAppConfigCachedTor].afterAll()
  }
}
