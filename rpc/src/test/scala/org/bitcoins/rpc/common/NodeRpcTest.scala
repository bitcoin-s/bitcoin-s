package org.bitcoins.rpc.common

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.rpc.TestUtil
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.scalatest.{ AsyncFlatSpec, BeforeAndAfterAll }

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, ExecutionContext }

class NodeRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("NodeRpcTest")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = TestUtil.network

  val client: BitcoindRpcClient = new BitcoindRpcClient(TestUtil.instance())

  override def beforeAll(): Unit = {
    TestUtil.startServers(client)
    Await.result(client.generate(200), 3.seconds)
  }

  override protected def afterAll(): Unit = {
    TestUtil.stopServers()
    Await.result(system.terminate(), 10.seconds)
  }

  behavior of "NodeRpc"

  it should "be able to abort a rescan of the blockchain" in {
    val rescanFailedF = recoverToSucceededIf[RuntimeException](client.rescanBlockChain())
    client.abortRescan().flatMap { _ =>
      rescanFailedF
    }
  }

  it should "be able to ping" in {
    client.ping().map(_ => succeed)
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
}

