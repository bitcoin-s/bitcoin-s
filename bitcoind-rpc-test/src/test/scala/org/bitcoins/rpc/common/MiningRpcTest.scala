package org.bitcoins.rpc.common

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.rpc.{BitcoindRpcTestConfig, BitcoindRpcTestUtil}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.AddNodeArgument
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.concurrent.{Await, ExecutionContext}

class MiningRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("MiningRpcTest")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  val client: BitcoindRpcClient = new BitcoindRpcClient(
    BitcoindRpcTestUtil.instance())
  val otherClient = new BitcoindRpcClient(BitcoindRpcTestUtil.instance())

  override def beforeAll(): Unit = {
    import BitcoindRpcTestConfig.DEFAULT_TIMEOUT

    val startF = BitcoindRpcTestUtil.startServers(Vector(client, otherClient))
    Await.result(startF, DEFAULT_TIMEOUT)

    val addNodeF =
      client.addNode(otherClient.getDaemon.uri, AddNodeArgument.Add)
    Await.result(addNodeF, DEFAULT_TIMEOUT)

    Await.result(client.generate(200), DEFAULT_TIMEOUT)
  }

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(Vector(client, otherClient))
    TestKit.shutdownActorSystem(system)
  }

  behavior of "MiningRpc"

  it should "be able to get a block template" in {
    val getBlockF = client.getBlockTemplate()
    getBlockF
      .recover {
        // getblocktemplate is having a bad time on regtest
        // https://github.com/bitcoin/bitcoin/issues/11379
        case err: Throwable
            if err.getMessage
              .contains("-9") =>
          succeed
        case other: Throwable => throw other
      }
      .map(_ => succeed)

  }

  it should "be able to generate blocks" in {
    client.generate(3).map { blocks =>
      assert(blocks.length == 3)
    }
  }

  it should "be able to get the mining info" in {
    client.getMiningInfo.map { info =>
      assert(info.chain == "regtest")
    }
  }

  it should "be able to generate blocks to an address" in {
    otherClient.getNewAddress.flatMap { address =>
      client.generateToAddress(3, address).flatMap { blocks =>
        assert(blocks.length == 3)
        blocks.foreach { hash =>
          client.getBlockWithTransactions(hash).map { block =>
            assert(
              block.tx.head.vout.head.scriptPubKey.addresses.get.head == address)
          }
        }
        succeed
      }
    }
  }

  it should "be able to generate blocks and then get their serialized headers" in {
    client.generate(2).flatMap { blocks =>
      client.getBlockHeaderRaw(blocks(1)).map { header =>
        assert(header.previousBlockHashBE == blocks(0))
      }
    }
  }

  it should "be able to generate blocks and then get their headers" in {
    client.generate(2).flatMap { blocks =>
      client.getBlockHeader(blocks(0)).map { header =>
        assert(header.nextblockhash.contains(blocks(1)))
      }
      client.getBlockHeader(blocks(1)).map { header =>
        assert(header.previousblockhash.contains(blocks(0)))
        assert(header.nextblockhash.isEmpty)
      }
    }
  }

  it should "be able to get the network hash per sec" in {
    client.getNetworkHashPS().map { hps =>
      assert(hps > 0)
    }
  }
}
