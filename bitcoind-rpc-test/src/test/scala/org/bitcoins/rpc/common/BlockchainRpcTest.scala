package org.bitcoins.rpc.common

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.rpc.{BitcoindRpcTestConfig, BitcoindRpcTestUtil}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.{AddNodeArgument, AddressType}
import org.bitcoins.rpc.util.AsyncUtil
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.async.Async.{async, await}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

class BlockchainRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {

  implicit val system: ActorSystem = ActorSystem("RpcClientTest_ActorSystem")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  implicit val client: BitcoindRpcClient = new BitcoindRpcClient(
    BitcoindRpcTestUtil.instance())
  val otherClient = new BitcoindRpcClient(BitcoindRpcTestUtil.instance())

  val pruneClient = new BitcoindRpcClient(
    BitcoindRpcTestUtil.instance(pruneMode = true))

  override def beforeAll(): Unit = {
    import BitcoindRpcTestConfig.DEFAULT_TIMEOUT

    val startF =
      BitcoindRpcTestUtil.startServers(Vector(client, otherClient, pruneClient))
    Await.result(startF, DEFAULT_TIMEOUT)

    val addNodeF =
      client.addNode(otherClient.getDaemon.uri, AddNodeArgument.Add)
    Await.result(addNodeF, DEFAULT_TIMEOUT)

    Await.result(client.generate(200), DEFAULT_TIMEOUT)

    val neededBlocks = 3500
    val rounds = 10
    (1 to rounds).foreach(
      _ =>
        Await.result(pruneClient.generate(neededBlocks / rounds),
                     DEFAULT_TIMEOUT))
    BitcoindRpcTestUtil.awaitConnection(client, otherClient)
  }

  override def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(Vector(client, otherClient, pruneClient))
    TestKit.shutdownActorSystem(system)
  }

  behavior of "BlockchainRpc"

  it should "be able to get the block count" in {
    // kick off both futures at the same time to avoid
    // one of them generating new blocks in between
    val clientCountF = client.getBlockCount
    val otherClientCountF = otherClient.getBlockCount

    for {
      List(clientCount, otherClientCount) <- Future.sequence(
        List(clientCountF, otherClientCountF))
    } yield {
      assert(clientCount >= 0)
      assert(clientCount == otherClientCount)
    }
  }

  it should "be able to get the first block" in {
    BitcoindRpcTestUtil.getFirstBlock.flatMap { block =>
      assert(block.tx.nonEmpty)
      assert(block.height == 1)
    }
  }

  it should "be able to prune the blockchain" in {
    pruneClient.getBlockCount.flatMap { count =>
      pruneClient.pruneBlockChain(count).flatMap { pruned =>
        assert(pruned > 0)
      }
    }
  }

  it should "be able to get blockchain info" in {
    client.getBlockChainInfo.flatMap { info =>
      assert(info.chain == "regtest")
      assert(info.softforks.length >= 3)
      assert(info.bip9_softforks.keySet.size >= 2)
      client.getBestBlockHash.map(bestHash =>
        assert(info.bestblockhash == bestHash))
    }
  }

  it should "be able to invalidate a block" in async {
    await(client.getBalance)
    val address =
      await(otherClient.getNewAddress(addressType = AddressType.P2SHSegwit))

    val txid =
      await(
        BitcoindRpcTestUtil
          .fundMemPoolTransaction(client, address, Bitcoins(1)))

    val blocks = await(client.generate(1))
    val mostRecentBlock = await(client.getBlock(blocks.head))
    assert(mostRecentBlock.tx.contains(txid))

    await(client.invalidateBlock(blocks.head))

    val mempool = await(client.getRawMemPool)
    assert(mempool.contains(txid))

    val count1 = await(client.getBlockCount)
    val count2 = await(otherClient.getBlockCount)

    await(client.generate(2)) // Ensure client and otherClient have the same blockchain
    assert(count1 == count2 - 1)
  }

  it should "be able to get block hash by height" in {
    client.generate(2).flatMap { blocks =>
      client.getBlockCount.flatMap { count =>
        client.getBlockHash(count).flatMap { hash =>
          assert(blocks(1) == hash)
          client.getBlockHash(count - 1).map { hash =>
            assert(blocks(0) == hash)
          }
        }
      }
    }
  }

  it should "be able to mark a block as precious" in {
    BitcoindRpcTestUtil.createNodePair().flatMap {
      case (client1, client2) =>
        client1.disconnectNode(client2.getDaemon.uri).flatMap { _ =>
          BitcoindRpcTestUtil.awaitDisconnected(client1, client2)
          client1.generate(1).flatMap { blocks1 =>
            client2.generate(1).flatMap { blocks2 =>
              client1.getBestBlockHash.flatMap { bestHash1 =>
                assert(bestHash1 == blocks1.head)
                client2.getBestBlockHash.flatMap { bestHash2 =>
                  assert(bestHash2 == blocks2.head)
                  client1
                    .addNode(client2.getDaemon.uri, AddNodeArgument.OneTry)
                    .flatMap { _ =>
                      AsyncUtil.awaitCondition(() =>
                        Try(Await.result(client2.preciousBlock(bestHash1),
                                         2.seconds)).isSuccess)

                      client2.getBestBlockHash.map { newBestHash =>
                        BitcoindRpcTestUtil.deleteNodePair(client1, client2)
                        assert(newBestHash == blocks1.head)
                      }
                    }
                }
              }
            }
          }
        }
    }
  }

  it should "be able to get tx out proof and verify it" in {
    BitcoindRpcTestUtil.getFirstBlock.flatMap { block =>
      client.getTxOutProof(Vector(block.tx.head.txid)).flatMap { merkle =>
        assert(merkle.transactionCount == UInt32(1))
        assert(merkle.hashes.length == 1)
        assert(merkle.hashes.head.flip == block.tx.head.txid)
        client.verifyTxOutProof(merkle).map { txids =>
          assert(block.tx.head.txid == txids.head)
        }
      }
    }
  }

  it should "be able to rescan the blockchain" in {
    client.rescanBlockChain().flatMap { result =>
      assert(result.start_height == 0)
      client.getBlockCount.map { count =>
        assert(count == result.stop_height)
      }
    }
  }

  it should "be able to get the chain tx stats" in {
    client.getChainTxStats.map { stats =>
      assert(stats.time > UInt32(0))
      assert(stats.txcount > 0)
      assert(stats.window_block_count > 0)
    }
  }

  it should "be able to get a raw block" in {
    client.generate(1).flatMap { blocks =>
      client.getBlockRaw(blocks(0)).flatMap { block =>
        client.getBlockHeaderRaw(blocks(0)).map { blockHeader =>
          assert(block.blockHeader == blockHeader)
        }
      }
    }
  }

  it should "be able to get a block" in {
    client.generate(1).flatMap { blocks =>
      client.getBlock(blocks(0)).flatMap { block =>
        assert(block.hash == blocks(0))
        assert(block.confirmations == 1)
        assert(block.size > 0)
        assert(block.weight > 0)
        assert(block.height > 0)
        assert(block.difficulty > 0)
      }
    }
  }

  it should "be able to get a transaction" in {
    BitcoindRpcTestUtil.getFirstBlock.flatMap { block =>
      client.getTransaction(block.tx.head.txid).flatMap { tx =>
        assert(tx.txid == block.tx.head.txid)
        assert(tx.amount == Bitcoins(50))
        assert(tx.blockindex.get == 0)
        assert(tx.details.head.category == "generate")
        assert(tx.generated.get)
        client.getBlockCount.map { count =>
          assert(tx.confirmations == count)
        }
      }
    }
  }

  it should "be able to get a block with verbose transactions" in {
    client.generate(2).flatMap { blocks =>
      client.getBlockWithTransactions(blocks(1)).flatMap { block =>
        assert(block.hash == blocks(1))
        assert(block.tx.length == 1)
        val tx = block.tx.head
        assert(tx.vout.head.n == 0)
      }
    }
  }

  it should "be able to get the chain tips" in {
    client.getChainTips.map { _ =>
      succeed
    }
  }

  it should "be able to get the best block hash" in {
    client.getBestBlockHash.map { _ =>
      succeed
    }
  }

  it should "be able to list all blocks since a given block" in {
    client.generate(3).flatMap { blocks =>
      client.listSinceBlock(blocks(0)).map { list =>
        assert(list.transactions.length >= 2)
        assert(list.transactions.exists(_.blockhash.contains(blocks(1))))
        assert(list.transactions.exists(_.blockhash.contains(blocks(2))))
      }
    }
  }

  it should "be able to verify the chain" in {
    client.verifyChain(blocks = 0).map { valid =>
      assert(valid)
    }
  }

  it should "be able to get the tx outset info" in {
    client.getTxOutSetInfo.flatMap { info =>
      client.getBlockCount.map { count =>
        assert(info.height == count)
      }
      client.getBestBlockHash.map { hash =>
        assert(info.bestblock == hash)
      }
    }
  }

  it should "be able to list transactions in a given range" in { // Assumes 30 transactions
    client.listTransactions().flatMap { list1 =>
      client.listTransactions(count = 20).flatMap { list2 =>
        assert(list2.takeRight(10) == list1)
        client.listTransactions(count = 20, skip = 10).map { list3 =>
          assert(list2.splitAt(10)._1 == list3.takeRight(10))
        }
      }
    }
  }

}
