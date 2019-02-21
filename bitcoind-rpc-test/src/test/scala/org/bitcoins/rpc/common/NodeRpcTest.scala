package org.bitcoins.rpc.common

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.number.UInt32
import org.bitcoins.rpc.{BitcoindRpcTestConfig, BitcoindRpcTestUtil}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.concurrent.{Await, ExecutionContext}

class NodeRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("NodeRpcTest")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  val client: BitcoindRpcClient = new BitcoindRpcClient(
    BitcoindRpcTestUtil.instance())

  override def beforeAll(): Unit = {
    import BitcoindRpcTestConfig.DEFAULT_TIMEOUT

    Await.result(BitcoindRpcTestUtil.startServers(Vector(client)),
                 DEFAULT_TIMEOUT)
    // generates more blocks than usual to ensure we don't rescan to fast to be able to abort
    (1 to 10).foreach(_ => Await.result(client.generate(200), DEFAULT_TIMEOUT))
  }

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(Vector(client))
    TestKit.shutdownActorSystem(system)
  }

  behavior of "NodeRpc"

  it should "be able to abort a rescan of the blockchain" in {
    val rescanFailedF =
      recoverToSucceededIf[RuntimeException](client.rescanBlockChain())
    client.abortRescan().flatMap { _ =>
      rescanFailedF
    }
  }

  it should "be able to ping" in {
    client.ping().map(_ => succeed)
  }

  it should "be able to get and set the logging configuration" in {
    client.logging.flatMap { info =>
      info.keySet.foreach(category => assert(info(category) == 1))
      client.logging(exclude = Vector("qt")).map { info =>
        assert(info("qt") == 0)
      }
    }
  }

  it should "be able to get the memory info" in {
    client.getMemoryInfo.map { info =>
      assert(info.locked.used > 0)
      assert(info.locked.free > 0)
      assert(info.locked.total > 0)
      assert(info.locked.locked > 0)
      assert(info.locked.chunks_used > 0)
    }
  }

  it should "be able to get the client's uptime" in {
    client.uptime.flatMap { time1 =>
      assert(time1 > UInt32(0))
    }
  }

  it should "be able to get help from bitcoind" in {
    client.help().flatMap { genHelp =>
      assert(!genHelp.isEmpty)
      client.help("help").map { helpHelp =>
        assert(genHelp != helpHelp)
        assert(!helpHelp.isEmpty)
      }
    }
  }
}
