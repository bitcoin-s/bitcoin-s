package org.bitcoins.rpc.v24

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.{
  AddressType,
  WalletFlag
}
import org.bitcoins.commons.jsonmodels.bitcoind.{
  AddressInfoResultPostV18,
  AddressInfoResultPostV21,
  AddressInfoResultPreV18,
  DescriptorsResult,
  GetBlockChainInfoResultPostV23
}
import org.bitcoins.core.api.chain.db.BlockHeaderDbHelper
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency._
import org.bitcoins.core.gcs.{BlockFilter, FilterType}
import org.bitcoins.core.protocol.{Bech32mAddress, BitcoinAddress}
import org.bitcoins.core.protocol.blockchain.RegTestNetChainParams
import org.bitcoins.core.protocol.script.descriptor.Descriptor
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.client.v24.BitcoindV24RpcClient
import org.bitcoins.testkit.chain.BlockHeaderHelper
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesFundedCachedV24,
  BitcoindRpcTestUtil
}

import java.io.File
import java.nio.file.Files
import java.time.Instant
import scala.concurrent.Future

class BitcoindV24RpcClientTest extends BitcoindFixturesFundedCachedV24 {

  val junkAddress: BitcoinAddress =
    BitcoinAddress("2NFyxovf6MyxfHqtVjstGzs6HeLqv92Nq4U")

  behavior of "BitcoindV24RpcClient"

  it should "get index info" in { client: BitcoindV24RpcClient =>
    def indexSynced(client: BitcoindRpcClient): Future[Boolean] = {
      client.getIndexInfo.map { indexes =>
        indexes("txindex").best_block_height == 101 && indexes(
          "basic block filter index"
        ).best_block_height == 101
      }
    }
    for {
      _ <- AsyncUtil.retryUntilSatisfiedF(() => indexSynced(client))
      indexes <- client.getIndexInfo
    } yield {
      val txIndexInfo = indexes("txindex")
      assert(txIndexInfo.synced)
      assert(txIndexInfo.best_block_height == 101)

      val blockFilterIndexInfo = indexes("basic block filter index")
      assert(blockFilterIndexInfo.synced)
      assert(blockFilterIndexInfo.best_block_height == 101)
    }
  }

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
        "pk(0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798)#gn28ywm7"
      )

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
        BitcoinAddress.fromString
      )
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
      Vector(
        "bcrt1qjqmxmkpmxt80xz4y3746zgt0q3u3ferr34acd5",
        "bcrt1qhku5rq7jz8ulufe2y6fkcpnlvpsta7rq4442dy",
        "bcrt1qpgptk2gvshyl0s9lqshsmx932l9ccsv265tvaq"
      )
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
    val imp = DescriptorsResult(
      desc = descriptor,
      timestamp = Instant.now().getEpochSecond,
      active = true,
      internal = None,
      range = Some(Vector(0, 2)),
      next = None
    )

    val resultF = client.importDescriptors(Vector(imp))

    for {
      result <- resultF
      _ = assert(result.forall(_.success))
      firstAddress <- client.getNewAddress
      secondAddress <- client.getNewAddress
      // check it by deriving addresses externally
      deriveAddresses <- client
        .deriveAddresses(descriptor, Some(Vector(0, 1)))
        .map(_.addresses)
    } yield {
      assert(Vector(firstAddress, secondAddress) == deriveAddresses)
    }
  }

  it should "be able to get the address info for a given address" in {
    client: BitcoindV24RpcClient =>
      for {
        addr <- client.getNewAddress
        info <- client.getAddressInfo(addr)
      } yield assert(info.address == addr)
  }

  it should "get a block filter given a block hash" in {
    client: BitcoindV24RpcClient =>
      for {
        blocks <- client.generate(1)
        blockHashBE = blocks.head
        blockFilter <- client.getBlockFilter(blockHashBE, FilterType.Basic)
        block <- client.getBlockRaw(blockHashBE)
        prevOuts = block.transactions
          .filterNot(_.isCoinbase)
          .flatMap(_.inputs.map(_.previousOutput))
        fundingOutputs <- Future.traverse(prevOuts) { outpoint =>
          client
            .getTransaction(outpoint.txIdBE)
            .map(_.hex.outputs(outpoint.idx))
        }
        prevFilter <- client.getBlockFilter(
          block.blockHeader.previousBlockHashBE,
          FilterType.Basic
        )
      } yield {
        val pubKeys = fundingOutputs.map(_.scriptPubKey).toVector
        val filter = BlockFilter(block, pubKeys)
        assert(filter.hash == blockFilter.filter.hash)
        assert(
          blockFilter.header == filter
            .getHeader(prevFilter.header.flip)
            .hashBE
        )
      }
  }

  it should "be able to get the balances" in { client: BitcoindV24RpcClient =>
    for {
      immatureBalance <- client.getBalances
      _ <- client.generate(1)
      newImmatureBalance <- client.getBalances
    } yield {
      val blockReward = 50
      assert(immatureBalance.mine.immature.toBigDecimal >= 0)
      assert(
        immatureBalance.mine.trusted.toBigDecimal + blockReward == newImmatureBalance.mine.trusted.toBigDecimal
      )
    }
  }

  it should "be able to get blockchain info" in {
    client: BitcoindV24RpcClient =>
      for {
        info <- client.getBlockChainInfo
        bestHash <- client.getBestBlockHash()
      } yield {
        assert(info.isInstanceOf[GetBlockChainInfoResultPostV23])
        val postV23Info = info.asInstanceOf[GetBlockChainInfoResultPostV23]
        assert(postV23Info.chain == RegTest)
        assert(postV23Info.bestblockhash == bestHash)
      }
  }

  it should "be able to get a block with verbose transactions" in {
    client: BitcoindRpcClient =>
      for {
        blocks <- client.generate(2)
        block <- client.getBlockWithTransactions(blocks(1))
      } yield {
        assert(block.hash == blocks(1))
        assert(block.tx.length == 1)
        val tx = block.tx.head
        assert(tx.vout.head.n == 0)
      }
  }

  it should "be able to set the wallet flag 'avoid_reuse'" in {
    client: BitcoindV24RpcClient =>
      for {
        unspentPre <- client.listUnspent
        result <- client.setWalletFlag(WalletFlag.AvoidReuse, value = true)
        unspentPost <- client.listUnspent
      } yield {
        assert(result.flag_name == "avoid_reuse")
        assert(result.flag_state)
        assert(unspentPre.forall(utxo => utxo.reused.isEmpty))
        assert(unspentPost.forall(utxo => utxo.reused.isDefined))
      }
  }

  it should "create a wallet with a passphrase" in {
    client: BitcoindV24RpcClient =>
      for {
        _ <- client.createWallet("suredbits", passphrase = "stackingsats")
        wallets <- client.listWallets
      } yield {
        assert(wallets.contains("suredbits"))
      }

  }

  it should "check to see if the utxoUpdate input has been updated" in {
    client: BitcoindV24RpcClient =>
      val descriptor =
        "pk(0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798)"

      val psbt =
        PSBT.fromBase64(
          "cHNidP8BACoCAAAAAAFAQg8AAAAAABepFG6Rty1Vk+fUOR4v9E6R6YXDFkHwhwAAAAAAAA=="
        )

      for {
        result <- client.utxoUpdatePsbt(psbt, Seq(descriptor))
      } yield {
        assert(result == psbt)
      }
  }

  it should "correct create multisig and get its descriptor" in {
    client: BitcoindV24RpcClient =>
      val pubKey1 = ECPublicKey.freshPublicKey
      val pubKey2 = ECPublicKey.freshPublicKey

      for {
        multiSigResult <- client.createMultiSig(
          2,
          Vector(pubKey1, pubKey2),
          AddressType.Bech32
        )
      } yield {
        // just validate we are able to receive a sane descriptor
        // no need to check checksum
        assert(
          multiSigResult.descriptor.startsWith(
            s"wsh(multi(2,${pubKey1.hex},${pubKey2.hex}))#"
          )
        )
      }
  }

  it should "correctly dump tx out set" in { client: BitcoindV24RpcClient =>
    for {
      hash <- client.getBestBlockHash()
      height <- client.getBestHashBlockHeight()
      result <- client.dumpTxOutSet(new File("utxo.dat").toPath)
    } yield {
      assert(Files.exists(result.path))
      // Mild clean up
      Files.delete(result.path)

      assert(result.base_hash == hash)
      assert(result.base_height == height)
      assert(result.coins_written > 0)
    }
  }

  it should "correct generate to a descriptor" in {
    client: BitcoindV24RpcClient =>
      // 2-of-2 multisig descriptor
      val descriptor =
        "sh(sortedmulti(2,023f720438186fbdfde0c0a403e770a0f32a2d198623a8a982c47b621f8b307640,03ed261094d609d5e02ba6553c2d91e4fd056006ce2fe64aace72b69cb5be3ab9c))#nj9wx7up"
      val numBlocks = 10
      for {
        hashes <- client.generateToDescriptor(numBlocks, descriptor)
      } yield assert(hashes.size == numBlocks)
  }

  it should "be able to get utxo info" in { client: BitcoindRpcClient =>
    for {
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
      info1 <- client.getTxOut(block.tx.head.txid, 0)
    } yield assert(info1.coinbase)
  }

  it should "generate a bech32m address" in { client: BitcoindRpcClient =>
    for {
      address <- client.getNewAddress(addressType = AddressType.Bech32m)
    } yield {
      assert(address.isInstanceOf[Bech32mAddress])
    }
  }
}
