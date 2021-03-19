package org.bitcoins.rpc.v18

import org.bitcoins.commons.jsonmodels.bitcoind.{
  AddressInfoResultPostV18,
  AddressInfoResultPostV21,
  AddressInfoResultPreV18
}
import org.bitcoins.core.api.chain.db.BlockHeaderDbHelper
import org.bitcoins.core.protocol.blockchain.RegTestNetChainParams
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.testkit.chain.BlockHeaderHelper
import org.bitcoins.testkit.rpc.BitcoindFixturesFundedCachedV18

class BitcoindV18RpcClientTest extends BitcoindFixturesFundedCachedV18 {

  behavior of "BitcoindV18RpcClient"

  it should "have extra address information" in { client =>
    for {
      address <- client.getNewAddress
      info <- client.getAddressInfo(address)
    } yield {
      info match {
        case _: AddressInfoResultPreV18 | _: AddressInfoResultPostV21 =>
          fail("Was expecting AddressInfoResultPostV18")
        case postV18Info: AddressInfoResultPostV18 =>
          assert(postV18Info.address == address)
      }
    }
  }

  it should "be able to start a V18 bitcoind instance" in { client =>
    assert(client.version == BitcoindVersion.V18)
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
