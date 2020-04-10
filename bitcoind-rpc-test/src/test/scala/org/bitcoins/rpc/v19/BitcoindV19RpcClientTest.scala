package org.bitcoins.rpc.v19
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.WalletFlag
import org.bitcoins.core.gcs.{BlockFilter, FilterType}
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest

import scala.concurrent.Future

class BitcoindV19RpcClientTest extends BitcoindRpcTest {
  lazy val clientF: Future[BitcoindV19RpcClient] = {
    val client = new BitcoindV19RpcClient(BitcoindRpcTestUtil.v19Instance())
    val clientIsStartedF = BitcoindRpcTestUtil.startServers(Vector(client))
    clientIsStartedF.map(_ => client)
  }
  lazy val clientPairF: Future[(BitcoindV19RpcClient, BitcoindV19RpcClient)] =
    BitcoindRpcTestUtil.createNodePairV19(clientAccum)

  clientF.foreach(c => clientAccum.+=(c))

  behavior of "BitcoindV19RpcClient"

  it should "be able to start a V19 bitcoind instance" in {

    clientF.map { client =>
      assert(client.version == BitcoindVersion.V19)
    }

  }

  it should "get a block filter given a block hash" in {
    for {
      (client, _) <- clientPairF
      blocks <- client.getNewAddress.flatMap(client.generateToAddress(1, _))
      blockFilter <- client.getBlockFilter(blocks.head, FilterType.Basic)

      block <- client.getBlockRaw(blocks.head)
      txs <- Future.sequence(
        block.transactions
          .filterNot(_.isCoinbase)
          .map(tx => client.getTransaction(tx.txIdBE)))

      prevFilter <- client.getBlockFilter(block.blockHeader.previousBlockHashBE,
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

  it should "be able to get the balances" in {
    for {
      (client, _) <- clientPairF
      immatureBalance <- client.getBalances
      _ <- client.getNewAddress.flatMap(client.generateToAddress(1, _))
      newImmatureBalance <- client.getBalances
    } yield {
      val blockReward = 12.5
      assert(immatureBalance.mine.immature.toBigDecimal >= 0)
      assert(
        immatureBalance.mine.immature.toBigDecimal + blockReward == newImmatureBalance.mine.immature.toBigDecimal)
    }
  }

  it should "be able to set the wallet flag 'avoid_reuse'" in {
    for {
      (client, _) <- clientPairF
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
    for {
      (client, _) <- clientPairF
      _ <- client.createWallet("suredbits", passphrase = "stackingsats")
      wallets <- client.listWallets
    } yield {
      assert(wallets.contains("suredbits"))
    }

  }

  it should "check to see if the utxoUpdate input has been updated" in {

    val descriptor =
      "pk(0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798)"

    val psbt =
      "cHNidP8BACoCAAAAAAFAQg8AAAAAABepFG6Rty1Vk+fUOR4v9E6R6YXDFkHwhwAAAAAAAA=="
    val updatedF =
      clientF.flatMap(client => client.utxoUpdatePsbt(psbt, Seq(descriptor)))

    updatedF.map { result =>
      assert(result.contains(psbt))
    }
  }
}
