package org.bitcoins.rpc.v23

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.core.protocol.Bech32mAddress
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.client.v23.BitcoindV23RpcClient
import org.bitcoins.testkit.rpc.BitcoindFixturesFundedCachedV23

class BitcoindV23RpcClientTest extends BitcoindFixturesFundedCachedV23 {

  behavior of "BitcoindV23RpcClient"

  it should "be able to start a V23 bitcoind instance" in {
    client: BitcoindV23RpcClient =>
      for {
        v <- client.version
      } yield assert(v == BitcoindVersion.V23)
  }

  it should "be able to get network info" in {
    freshClient: BitcoindV23RpcClient =>
      for {
        info <- freshClient.getNetworkInfo
      } yield {
        assert(info.networkactive)
        assert(info.localrelay)
      }
  }

  it should "generate a bech32m address" in { client: BitcoindV23RpcClient =>
    for {
      address <- client.getNewAddress(addressType = AddressType.Bech32m)
    } yield {
      assert(address.isInstanceOf[Bech32mAddress])
    }
  }
}
