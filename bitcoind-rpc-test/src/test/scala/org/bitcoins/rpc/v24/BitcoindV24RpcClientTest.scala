package org.bitcoins.rpc.v24

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.commons.jsonmodels.bitcoind.{
  AddressInfoResultPostV18,
  AddressInfoResultPostV21,
  AddressInfoResultPreV18
}
import org.bitcoins.core.api.chain.db.BlockHeaderDbHelper
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.{Bech32mAddress, BitcoinAddress}
import org.bitcoins.core.protocol.blockchain.RegTestNetChainParams
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.client.v24.BitcoindV24RpcClient
import org.bitcoins.testkit.chain.BlockHeaderHelper
import org.bitcoins.testkit.rpc.BitcoindFixturesFundedCachedV24

class BitcoindV24RpcClientTest extends BitcoindFixturesFundedCachedV24 {

  val junkAddress: BitcoinAddress =
    BitcoinAddress("2NFyxovf6MyxfHqtVjstGzs6HeLqv92Nq4U")

  behavior of "BitcoindV24RpcClient"

  it should "be able to start a V24 bitcoind instance" in {
    client: BitcoindV24RpcClient =>
      for {
        v <- client.version
      } yield assert(v == BitcoindVersion.V24)
  }

  it should "be able to get network info" in {
    freshClient: BitcoindV24RpcClient =>
      for {
        info <- freshClient.getNetworkInfo
      } yield {
        assert(info.networkactive)
        assert(info.localrelay)
      }
  }

  it should "generate a bech32m address" in { client: BitcoindV24RpcClient =>
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

  it should "simulate a transaction" in { client =>
    for {
      txid <- client.sendToAddress(junkAddress, Bitcoins.one)
      tx <- client.getRawTransaction(txid).map(_.hex)
      change <- client.simulateRawTransaction(tx)
    } yield assert(change <= -Bitcoins.one) // 1 bitcoin + fees
  }

  it should "get tx spending prev out" in { client =>
    for {
      txid <- client.sendToAddress(junkAddress, Bitcoins.one)
      tx <- client.getRawTransaction(txid).map(_.hex)
      spending <- client.getTxSpendingPrevOut(tx.inputs.head.previousOutput)
    } yield assert(spending.spendingtxid.contains(txid))
  }
}
