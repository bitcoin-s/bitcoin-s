package org.bitcoins.rpc.common

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.rpc.BitcoindRpcTestUtil
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.{ AddNodeArgument, AddressType }
import org.scalatest.{ AsyncFlatSpec, BeforeAndAfterAll }

import scala.async.Async.{ async, await }
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, ExecutionContext }

class BlockchainRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {

  implicit val system: ActorSystem = ActorSystem("RpcClientTest_ActorSystem")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  implicit val client: BitcoindRpcClient = new BitcoindRpcClient(BitcoindRpcTestUtil.instance())
  val otherClient = new BitcoindRpcClient(BitcoindRpcTestUtil.instance())
  val clients = Vector(client, otherClient)

  override def beforeAll(): Unit = {
    BitcoindRpcTestUtil.startServers(clients)
    Await.result(client.addNode(otherClient.getDaemon.uri, AddNodeArgument.Add), 3.seconds)
    Await.result(client.generate(200), 3.seconds)
    BitcoindRpcTestUtil.awaitConnection(client, otherClient)
  }

  override def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(clients)
    Await.result(system.terminate(), 10.seconds)
  }

  behavior of "BlockchainRpc"

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
    val address = await(otherClient.getNewAddress(addressType = AddressType.P2SHSegwit))
    val txid = await(client.sendToAddress(address, Bitcoins(1)))
    val oldMempool = await(client.getRawMemPool)
    val blocks = await(client.generate(1))
    val mostRecentBlock = await(client.getBlock(blocks.head))
    assert(mostRecentBlock.tx.contains(txid))
    await(client.invalidateBlock(blocks.head))
    val mempool = await(client.getRawMemPool)
    assert(mempool.contains(txid))
    val count1 = await(client.getBlockCount)
    val count2 = await(otherClient.getBlockCount)
    client.generate(2) // Ensure client and otherClient have the same blockchain
    assert(count1 == count2 - 1)
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

}
