package org.bitcoins.rpc.v24

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.commons.jsonmodels.bitcoind.{
  AddressInfoResultPostV18,
  AddressInfoResultPostV21,
  AddressInfoResultPreV18,
  DescriptorsResult
}
import org.bitcoins.core.api.chain.db.BlockHeaderDbHelper
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.{Bech32mAddress, BitcoinAddress}
import org.bitcoins.core.protocol.blockchain.RegTestNetChainParams
import org.bitcoins.core.protocol.script.descriptor.Descriptor
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.client.v24.BitcoindV24RpcClient
import org.bitcoins.testkit.chain.BlockHeaderHelper
import org.bitcoins.testkit.rpc.BitcoindFixturesFundedCachedV24

import java.time.Instant

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
      Descriptor.fromString(
        "pk(0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798)#gn28ywm7")

    val descriptorF = client.getDescriptorInfo(descriptor)

    descriptorF.map { result =>
      assert(result.descriptor == descriptor)
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

  it should "derive addresses from a descriptor" in { client =>
    val str0 =
      "wpkh(tprv8ZgxMBicQKsPd7Uf69XL1XwhmjHopUGep8GuEiJDZmbQz6o58LninorQAfcKZWARbtRtfnLcJ5MQ2AtHcQJCCRUcMRvmDUjyEmNUWwx8UbK/1/1/0)#t6wfjs64"
    val descriptor0 = Descriptor.fromString(str0)
    assert(descriptor0.toString == str0)
    val addresses0F =
      client.deriveAddresses(descriptor0, None).map(_.addresses)
    val expected0 =
      Vector("bcrt1qjqmxmkpmxt80xz4y3746zgt0q3u3ferr34acd5").map(
        BitcoinAddress.fromString)
    val assert0 = addresses0F.map { addresses =>
      assert(addresses == expected0)
    }

    val str1 =
      "wpkh(tprv8ZgxMBicQKsPd7Uf69XL1XwhmjHopUGep8GuEiJDZmbQz6o58LninorQAfcKZWARbtRtfnLcJ5MQ2AtHcQJCCRUcMRvmDUjyEmNUWwx8UbK/1/1/*)#kft60nuy"

    val descriptor1 = Descriptor.fromString(str1)
    assert(descriptor1.toString == str1)
    val addresses1F =
      client.deriveAddresses(descriptor1, Some(Vector(0, 2))).map(_.addresses)
    val expected1 =
      Vector("bcrt1qjqmxmkpmxt80xz4y3746zgt0q3u3ferr34acd5",
             "bcrt1qhku5rq7jz8ulufe2y6fkcpnlvpsta7rq4442dy",
             "bcrt1qpgptk2gvshyl0s9lqshsmx932l9ccsv265tvaq")
        .map(BitcoinAddress.fromString)

    val assert1 = assert0.flatMap(_ =>
      addresses1F.map { addresses =>
        assert(addresses == expected1)
      })

    assert1
  }

  it must "importdescriptors" in { client =>
    val str1 =
      "wpkh(tprv8ZgxMBicQKsPd7Uf69XL1XwhmjHopUGep8GuEiJDZmbQz6o58LninorQAfcKZWARbtRtfnLcJ5MQ2AtHcQJCCRUcMRvmDUjyEmNUWwx8UbK/1/1/*)#kft60nuy"
    val descriptor = Descriptor.fromString(str1)
    val imp = DescriptorsResult(desc = descriptor,
                                timestamp = Instant.now().getEpochSecond,
                                active = true,
                                internal = None,
                                range = Some(Vector(0, 2)),
                                next = None)

    val resultF = client.importDescriptors(Vector(imp))

    for {
      result <- resultF
      _ = assert(result.forall(_.success))
      firstAddress <- client.getNewAddress
      secondAddress <- client.getNewAddress
      //check it by deriving addresses externally
      deriveAddresses <- client
        .deriveAddresses(descriptor, Some(Vector(0, 1)))
        .map(_.addresses)
    } yield {
      assert(Vector(firstAddress, secondAddress) == deriveAddresses)
    }
  }
}
