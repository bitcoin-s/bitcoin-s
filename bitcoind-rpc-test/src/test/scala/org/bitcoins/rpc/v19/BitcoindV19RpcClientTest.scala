package org.bitcoins.rpc.v19

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.WalletFlag
import org.bitcoins.commons.jsonmodels.bitcoind.{
  Bip9SoftforkPostV19,
  GetBlockChainInfoResultPostV19
}
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.gcs.{BlockFilter, FilterType}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.testkit.rpc.BitcoindFixturesFundedCachedV19

import scala.concurrent.Future

class BitcoindV19RpcClientTest extends BitcoindFixturesFundedCachedV19 {

  behavior of "BitcoindV19RpcClient"

  it should "be able to start a V19 bitcoind instance" in {
    client: BitcoindV19RpcClient =>
      assert(client.version == BitcoindVersion.V19)
  }

  it should "get a block filter given a block hash" in {
    client: BitcoindV19RpcClient =>
      for {
        blocks <- client.getNewAddress.flatMap(client.generateToAddress(1, _))
        blockFilter <- client.getBlockFilter(blocks.head, FilterType.Basic)

        block <- client.getBlockRaw(blocks.head)
        txs <- Future.sequence(
          block.transactions
            .filterNot(_.isCoinbase)
            .map(tx => client.getTransaction(tx.txIdBE)))

        prevFilter <- client.getBlockFilter(
          block.blockHeader.previousBlockHashBE,
          FilterType.Basic)
      } yield {
        val pubKeys = txs.flatMap(_.hex.outputs.map(_.scriptPubKey)).toVector
        val filter = BlockFilter(block, pubKeys)
        assert(filter.hash == blockFilter.filter.hash)
        assert(
          blockFilter.header == filter
            .getHeader(prevFilter.header.flip)
            .hash
            .flip)
      }
  }

  it should "be able to get the balances" in { client: BitcoindV19RpcClient =>
    for {
      immatureBalance <- client.getBalances
      _ <- client.getNewAddress.flatMap(client.generateToAddress(1, _))
      newImmatureBalance <- client.getBalances
    } yield {
      val blockReward = 50
      assert(immatureBalance.mine.immature.toBigDecimal >= 0)
      assert(
        immatureBalance.mine.trusted.toBigDecimal + blockReward == newImmatureBalance.mine.trusted.toBigDecimal)
    }
  }

  it should "be able to get blockchain info" in {
    client: BitcoindV19RpcClient =>
      for {
        info <- client.getBlockChainInfo
        bestHash <- client.getBestBlockHash
      } yield {
        assert(info.isInstanceOf[GetBlockChainInfoResultPostV19])
        val preV19Info = info.asInstanceOf[GetBlockChainInfoResultPostV19]
        assert(preV19Info.chain == RegTest)
        assert(preV19Info.softforks.size >= 5)
        assert(
          preV19Info.softforks.values.exists(
            _.isInstanceOf[Bip9SoftforkPostV19]))
        assert(preV19Info.bestblockhash == bestHash)
      }
  }

  it should "be able to set the wallet flag 'avoid_reuse'" in {
    client: BitcoindV19RpcClient =>
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
    client: BitcoindV19RpcClient =>
      for {
        _ <- client.createWallet("suredbits", passphrase = "stackingsats")
        wallets <- client.listWallets
      } yield {
        assert(wallets.contains("suredbits"))
      }

  }

  it should "check to see if the utxoUpdate input has been updated" in {
    client: BitcoindV19RpcClient =>
      val descriptor =
        "pk(0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798)"

      val psbt =
        PSBT.fromBase64(
          "cHNidP8BACoCAAAAAAFAQg8AAAAAABepFG6Rty1Vk+fUOR4v9E6R6YXDFkHwhwAAAAAAAA==")

      for {
        result <- client.utxoUpdatePsbt(psbt, Seq(descriptor))
      } yield {
        assert(result == psbt)
      }
  }
}
