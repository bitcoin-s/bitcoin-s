
package org.bitcoins.rpc.common

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.rpc.TestUtil
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.AddNodeArgument
import org.scalatest.{ Assertion, AsyncFlatSpec, BeforeAndAfterAll }

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, ExecutionContext }

class MiningRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("MiningRpcTest")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = TestUtil.network

  val client: BitcoindRpcClient = new BitcoindRpcClient(TestUtil.instance())
  val otherClient = new BitcoindRpcClient(TestUtil.instance())

  override def beforeAll(): Unit = {
    TestUtil.startServers(client, otherClient)
    Await.result(client.addNode(otherClient.getDaemon.uri, AddNodeArgument.Add), 3.seconds)
    Await.result(client.generate(200), 3.seconds)
  }

  override protected def afterAll(): Unit = {
    TestUtil.stopServers()
    Await.result(system.terminate(), 10.seconds)
  }

  behavior of "MiningRpc"

  it should "be able to get a block template" in {
    client.getBlockTemplate().failed.map {
      // getblocktemplate is having a bad time on regtest
      // https://github.com/bitcoin/bitcoin/issues/11379
      case err: Throwable if err.getMessage
        .contains("-9") => succeed
    }
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
