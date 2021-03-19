package org.bitcoins.rpc.common

import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockChainInfoResultPreV19
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesCachedPairV17,
  BitcoindRpcTestUtil
}

import scala.concurrent.{Await, Future}

class BlockchainRpcTest extends BitcoindFixturesCachedPairV17 {

  lazy val pruneClientF: Future[BitcoindRpcClient] = {
    val pruneClient =
      BitcoindRpcClient.withActorSystem(
        BitcoindRpcTestUtil
          .instance(pruneMode = true, versionOpt = Some(BitcoindVersion.V17)))

    for {
      _ <- pruneClient.start()
      _ <- pruneClient.getNewAddress.flatMap(
        pruneClient.generateToAddress(1000, _))
    } yield pruneClient
  }

  behavior of "BlockchainRpc"

  it should "be able to get the first block" in { nodePair =>
    val client = nodePair.node1
    for {
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
    } yield {
      assert(block.tx.nonEmpty)
      assert(block.height == 1)
    }
  }

  it should "be able to get blockchain info" in { nodePair =>
    val client = nodePair.node1
    for {
      info <- client.getBlockChainInfo
      bestHash <- client.getBestBlockHash
    } yield {
      assert(info.isInstanceOf[GetBlockChainInfoResultPreV19])
      val preV19Info = info.asInstanceOf[GetBlockChainInfoResultPreV19]
      assert(preV19Info.chain == RegTest)
      assert(preV19Info.softforks.length >= 3)
      assert(preV19Info.bip9_softforks.keySet.size >= 2)
      assert(preV19Info.bestblockhash == bestHash)
    }
  }

  it should "be able to invalidate a block" in { nodePair =>
    val client = nodePair.node1
    val otherClient = nodePair.node2
    for {
      address <- otherClient.getNewAddress(addressType = AddressType.P2SHSegwit)
      txid <-
        BitcoindRpcTestUtil
          .fundMemPoolTransaction(client, address, Bitcoins(1))
      blocks <- client.getNewAddress.flatMap(client.generateToAddress(1, _))
      mostRecentBlock <- client.getBlock(blocks.head)
      _ <- client.invalidateBlock(blocks.head)
      mempool <- client.getRawMemPool
      count1 <- client.getBlockCount
      count2 <- otherClient.getBlockCount

      _ <- client.getNewAddress.flatMap(
        client.generateToAddress(2, _)
      ) // Ensure client and otherClient have the same blockchain
    } yield {
      assert(mostRecentBlock.tx.contains(txid))
      assert(mempool.contains(txid))
      assert(count1 == count2 - 1)
    }
  }

  it should "be able to get block hash by height" in { nodePair =>
    val client = nodePair.node1
    for {
      blocks <- client.getNewAddress.flatMap(client.generateToAddress(2, _))
      count <- client.getBlockCount
      hash <- client.getBlockHash(count)
      prevhash <- client.getBlockHash(count - 1)
    } yield {
      assert(blocks(1) == hash)
      assert(blocks(0) == prevhash)
    }
  }

  it should "be able to get tx out proof and verify it" in { nodePair =>
    val client = nodePair.node1
    for {
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
      merkle <- client.getTxOutProof(Vector(block.tx.head.txid))
      txids <- client.verifyTxOutProof(merkle)
    } yield {
      assert(merkle.transactionCount == UInt32(1))
      assert(merkle.hashes.length == 1)
      assert(merkle.hashes.head.flip == block.tx.head.txid)
      assert(block.tx.head.txid == txids.head)
    }
  }

  it should "be able to rescan the blockchain" in { nodePair =>
    val client = nodePair.node1
    for {
      result <- client.rescanBlockChain()
      count <- client.getBlockCount
    } yield {
      assert(result.start_height == 0)
      assert(count == result.stop_height)
    }
  }

  it should "be able to get the chain tx stats" in { nodePair =>
    val client = nodePair.node1
    for {
      stats <- client.getChainTxStats
    } yield {
      assert(stats.txcount > 0)
      assert(stats.window_block_count > 0)
    }
  }

  it should "be able to get a raw block" in { nodePair =>
    val client = nodePair.node1
    for {
      blocks <- client.getNewAddress.flatMap(client.generateToAddress(1, _))
      block <- client.getBlockRaw(blocks.head)
      blockHeader <- client.getBlockHeaderRaw(blocks.head)
    } yield assert(block.blockHeader == blockHeader)
  }

  it should "be able to get a block" in { nodePair =>
    val client = nodePair.node1
    for {
      blocks <- client.getNewAddress.flatMap(client.generateToAddress(1, _))
      block <- client.getBlock(blocks.head)
    } yield {
      assert(block.hash == blocks(0))
      assert(block.confirmations == 1)
      assert(block.size > 0)
      assert(block.weight > 0)
      assert(block.height > 0)
      assert(block.difficulty > 0)
    }
  }

  it should "be able to get a transaction" in { nodePair =>
    val client = nodePair.node1
    for {
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
      tx <- client.getTransaction(block.tx.head.txid)
      count <- client.getBlockCount
    } yield {
      assert(tx.txid == block.tx.head.txid)
      assert(tx.amount == Bitcoins(50))
      assert(tx.blockindex.get == 0)
      assert(tx.details.head.category == "generate")
      assert(tx.generated.get)
      assert(tx.confirmations == count)
    }
  }

  it should "be able to get a block with verbose transactions" in { nodePair =>
    val client = nodePair.node1
    for {
      blocks <- client.getNewAddress.flatMap(client.generateToAddress(2, _))
      block <- client.getBlockWithTransactions(blocks(1))
    } yield {
      assert(block.hash == blocks(1))
      assert(block.tx.length == 1)
      val tx = block.tx.head
      assert(tx.vout.head.n == 0)
    }
  }

  it should "be able to get the chain tips" in { nodePair =>
    val client = nodePair.node1
    for {
      _ <- client.getChainTips
    } yield succeed
  }

  it should "be able to get the best block hash" in { nodePair =>
    val client = nodePair.node1
    for {
      _ <- client.getBestBlockHash
    } yield succeed
  }

  it should "be able to list all blocks since a given block" in { nodePair =>
    val client = nodePair.node1
    for {
      blocks <- client.getNewAddress.flatMap(client.generateToAddress(3, _))
      list <- client.listSinceBlock(blocks(0))
    } yield {
      assert(list.transactions.length >= 2)
      assert(list.transactions.exists(_.blockhash.contains(blocks(1))))
      assert(list.transactions.exists(_.blockhash.contains(blocks(2))))
    }
  }

  it should "be able to verify the chain" in { nodePair =>
    val client = nodePair.node1
    for {
      valid <- client.verifyChain(blocks = 0)
    } yield assert(valid)
  }

  it should "be able to get the tx outset info" in { nodePair =>
    val client = nodePair.node1
    for {
      info <- client.getTxOutSetInfo
      count <- client.getBlockCount
      hash <- client.getBestBlockHash
    } yield {
      assert(info.height == count)
      assert(info.bestblock == hash)
    }
  }

  it should "be able to prune the blockchain" in { _ =>
    for {
      pruneClient <- pruneClientF
      count <- pruneClient.getBlockCount
      pruned <- pruneClient.pruneBlockChain(count)
    } yield {
      assert(pruned > 0)
    }
  }

  override def afterAll(): Unit = {
    val stoppedF = pruneClientF.flatMap(BitcoindRpcTestUtil.stopServer)
    val _ = Await.result(stoppedF, duration)
    super.afterAll()
  }
}
