package org.bitcoins.rpc.common

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.{AddNodeArgument, AddressType}
import org.bitcoins.rpc.util.AsyncUtil
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest

import scala.concurrent.Future
import org.bitcoins.core.config.RegTest

class BlockchainRpcTest extends BitcoindRpcTest {

  lazy val clientsF: Future[(BitcoindRpcClient, BitcoindRpcClient)] =
    BitcoindRpcTestUtil.createNodePair(clientAccum = clientAccum)

  lazy val pruneClientF: Future[BitcoindRpcClient] = clientsF.flatMap {
    case (_, _) =>
      val pruneClient =
        new BitcoindRpcClient(BitcoindRpcTestUtil.instance(pruneMode = true))

      clientAccum += pruneClient

      for {
        _ <- pruneClient.start()
        _ <- pruneClient.generate(1000)
      } yield pruneClient
  }

  behavior of "BlockchainRpc"

  it should "be able to get the block count" in {

    for {
      (client, otherClient) <- clientsF

      // kick off both futures at the same time to avoid
      // one of them generating new blocks in between
      clientCountF = client.getBlockCount
      otherClientCountF = otherClient.getBlockCount
      List(clientCount, otherClientCount) <- {
        val countsF = List(clientCountF, otherClientCountF)
        Future.sequence(countsF)
      }
    } yield {
      assert(clientCount >= 0)
      assert(clientCount == otherClientCount)
    }
  }

  it should "be able to get the first block" in {
    for {
      (client, _) <- clientsF
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
    } yield {
      assert(block.tx.nonEmpty)
      assert(block.height == 1)
    }
  }

  it should "be able to prune the blockchain" in {
    for {
      pruneClient <- pruneClientF
      count <- pruneClient.getBlockCount
      pruned <- pruneClient.pruneBlockChain(count)
    } yield {
      assert(pruned > 0)
    }
  }

  it should "be able to get blockchain info" in {
    for {
      (client, _) <- clientsF
      info <- client.getBlockChainInfo
      bestHash <- client.getBestBlockHash
    } yield {
      assert(info.chain == RegTest)
      assert(info.softforks.length >= 3)
      assert(info.bip9_softforks.keySet.size >= 2)
      assert(info.bestblockhash == bestHash)
    }
  }

  it should "be able to invalidate a block" in {
    for {
      (client, otherClient) <- clientsF
      address <- otherClient.getNewAddress(addressType = AddressType.P2SHSegwit)
      txid <- BitcoindRpcTestUtil
        .fundMemPoolTransaction(client, address, Bitcoins(1))
      blocks <- client.generate(1)
      mostRecentBlock <- client.getBlock(blocks.head)
      _ <- client.invalidateBlock(blocks.head)
      mempool <- client.getRawMemPool
      count1 <- client.getBlockCount
      count2 <- otherClient.getBlockCount

      _ <- client.generate(2) // Ensure client and otherClient have the same blockchain
    } yield {
      assert(mostRecentBlock.tx.contains(txid))
      assert(mempool.contains(txid))
      assert(count1 == count2 - 1)
    }
  }

  it should "be able to get block hash by height" in {
    for {
      (client, _) <- clientsF
      blocks <- client.generate(2)
      count <- client.getBlockCount
      hash <- client.getBlockHash(count)
      prevhash <- client.getBlockHash(count - 1)
    } yield {
      assert(blocks(1) == hash)
      assert(blocks(0) == prevhash)
    }
  }

  it should "be able to mark a block as precious" in {
    for {
      (freshClient, otherFreshClient) <- BitcoindRpcTestUtil.createNodePair(
        clientAccum)
      _ <- freshClient.disconnectNode(otherFreshClient.getDaemon.uri)
      _ <- BitcoindRpcTestUtil.awaitDisconnected(freshClient, otherFreshClient)

      blocks1 <- freshClient.generate(1)
      blocks2 <- otherFreshClient.generate(1)

      bestHash1 <- freshClient.getBestBlockHash
      _ = assert(bestHash1 == blocks1.head)
      bestHash2 <- otherFreshClient.getBestBlockHash
      _ = assert(bestHash2 == blocks2.head)

      _ <- freshClient
        .addNode(otherFreshClient.getDaemon.uri, AddNodeArgument.OneTry)
      _ <- AsyncUtil.retryUntilSatisfiedF(() =>
        BitcoindRpcTestUtil.hasSeenBlock(otherFreshClient, bestHash1))

      _ <- otherFreshClient.preciousBlock(bestHash1)
      newBestHash <- otherFreshClient.getBestBlockHash

    } yield assert(newBestHash == bestHash1)
  }

  it should "be able to get tx out proof and verify it" in {
    for {
      (client, _) <- clientsF
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

  it should "be able to rescan the blockchain" in {
    for {
      (client, _) <- clientsF
      result <- client.rescanBlockChain()
      count <- client.getBlockCount
    } yield {
      assert(result.start_height == 0)
      assert(count == result.stop_height)
    }
  }

  it should "be able to get the chain tx stats" in {
    for {
      (client, _) <- clientsF
      stats <- client.getChainTxStats
    } yield {
      assert(stats.txcount > 0)
      assert(stats.window_block_count > 0)
    }
  }

  it should "be able to get a raw block" in {
    for {
      (client, _) <- clientsF
      blocks <- client.generate(1)
      block <- client.getBlockRaw(blocks.head)
      blockHeader <- client.getBlockHeaderRaw(blocks.head)
    } yield assert(block.blockHeader == blockHeader)
  }

  it should "be able to get a block" in {
    for {
      (client, _) <- clientsF
      blocks <- client.generate(1)
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

  it should "be able to get a transaction" in {
    for {
      (client, _) <- clientsF
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

  it should "be able to get a block with verbose transactions" in {
    for {
      (client, _) <- clientsF
      blocks <- client.generate(2)
      block <- client.getBlockWithTransactions(blocks(1))
    } yield {
      assert(block.hash == blocks(1))
      assert(block.tx.length == 1)
      val tx = block.tx.head
      assert(tx.vout.head.n == 0)
    }
  }

  it should "be able to get the chain tips" in {
    for {
      (client, _) <- clientsF
      _ <- client.getChainTips
    } yield succeed
  }

  it should "be able to get the best block hash" in {
    for {
      (client, _) <- clientsF
      _ <- client.getBestBlockHash
    } yield succeed
  }

  it should "be able to list all blocks since a given block" in {
    for {
      (client, _) <- clientsF
      blocks <- client.generate(3)
      list <- client.listSinceBlock(blocks(0))
    } yield {
      assert(list.transactions.length >= 2)
      assert(list.transactions.exists(_.blockhash.contains(blocks(1))))
      assert(list.transactions.exists(_.blockhash.contains(blocks(2))))
    }
  }

  it should "be able to verify the chain" in {
    for {
      (client, _) <- clientsF
      valid <- client.verifyChain(blocks = 0)
    } yield assert(valid)
  }

  it should "be able to get the tx outset info" in {
    for {
      (client, _) <- clientsF
      info <- client.getTxOutSetInfo
      count <- client.getBlockCount
      hash <- client.getBestBlockHash
    } yield {
      assert(info.height == count)
      assert(info.bestblock == hash)
    }
  }

  it should "be able to list transactions in a given range" in { // Assumes 30 transactions
    for {
      (client, _) <- clientsF
      list1 <- client.listTransactions()
      list2 <- client.listTransactions(count = 20)
      list3 <- client.listTransactions(count = 20, skip = 10)
    } yield {
      assert(list2.takeRight(10) == list1)
      assert(list2.splitAt(10)._1 == list3.takeRight(10))
    }
  }
}
