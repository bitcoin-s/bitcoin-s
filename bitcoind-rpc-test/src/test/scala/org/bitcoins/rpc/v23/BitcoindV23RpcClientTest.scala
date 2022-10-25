package org.bitcoins.rpc.v23

import org.bitcoins.commons.jsonmodels.bitcoind.{
  AddressInfoResultPostV18,
  AddressInfoResultPostV21,
  AddressInfoResultPreV18
}
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.core.api.chain.db.BlockHeaderDbHelper
import org.bitcoins.core.protocol.Bech32mAddress
import org.bitcoins.core.protocol.blockchain.RegTestNetChainParams
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.client.v23.BitcoindV23RpcClient
import org.bitcoins.testkit.chain.BlockHeaderHelper
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

  it should "have extra address information" in { client =>
    for {
      address <- client.getNewAddress
      info <- client.getAddressInfo(address)
    } yield {
      info match {
        case _: AddressInfoResultPreV18 | _: AddressInfoResultPostV18 =>
          fail("Was expecting AddressInfoResultPostV21")
        case postV21Info: AddressInfoResultPostV21 =>
          assert(postV21Info.address == address)
      }
    }
  }

  it should "return active rpc commands" in { client =>
    val generatedF =
      client.getNewAddress.flatMap(addr => client.generateToAddress(100, addr))
    val rpcinfoF =
      generatedF.flatMap(_ => client.getRpcInfo())

    rpcinfoF.map { result =>
      assert(result.active_commands.length == 1)
    }
  }

  it should "return a list of wallets" in { client =>
    for {
      _ <- client.createWallet("Suredbits")
      list <- client.listWalletDir()
    } yield {
      assert(list.wallets.exists(_.name.contains("Suredbits")))
    }
  }

  it should "analyze a descriptor" in { client =>
    val descriptor =
      "pk(0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798)"

    val descriptorF = client.getDescriptorInfo(descriptor)

    descriptorF.map { result =>
      assert(result.isrange.==(false))
      assert(result.issolvable.==(true))
      assert(result.hasprivatekeys.==(false))
    }
  }

  it should "get node address given a null parameter" in { client =>
    val nodeF = client.getNodeAddresses()

    nodeF.map { result =>
      assert(result.isEmpty)
    }
  }

  it should "successfully submit a header" in { client =>
    val genesisHeader = RegTestNetChainParams.genesisBlock.blockHeader
    val genesisHeaderDb =
      BlockHeaderDbHelper.fromBlockHeader(height = 1, BigInt(0), genesisHeader)
    val nextHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
    client.submitHeader(nextHeader.blockHeader).map(_ => succeed)
  }

}
