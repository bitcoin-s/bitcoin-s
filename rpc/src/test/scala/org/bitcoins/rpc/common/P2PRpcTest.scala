package org.bitcoins.rpc.common

import java.net.URI

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.number.UInt32
import org.bitcoins.rpc.TestUtil
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.AddNodeArgument
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.async.Async.{async, await}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}

class P2PRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("P2PRpcTest")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = TestUtil.network

  val client = new BitcoindRpcClient(TestUtil.instance())

  override protected def beforeAll(): Unit = {
    TestUtil.startServers(client)
  }

  override protected def afterAll(): Unit = {
    TestUtil.stopServers(client)
    Await.result(system.terminate(), 10.seconds)
  }

  behavior of "P2PRpcTest"

  it should "be able to get peer info" in async {
    val (freshClient, otherFreshClient) = await(TestUtil.createNodePair())
    val infoList = await(freshClient.getPeerInfo)
    assert(infoList.length == 1)

    val info = infoList.head
    assert(!info.inbound)
    assert(info.addnode)
    assert(info.networkInfo.addr == otherFreshClient.getDaemon.uri)
  }

  it should "be able to get the added node info" in async {
    val (freshClient, otherFreshClient) = await(TestUtil.createNodePair())
    val info = await(freshClient.getAddedNodeInfo)

    assert(info.length == 1)
    assert(info.head.addednode == otherFreshClient.getDaemon.uri)
    assert(info.head.connected.contains(true))

  }

  it should "be able to get the network info" in async {
    val (freshClient, otherFreshClient) = TestUtil.createUnconnectedNodePair()
    val info = await(freshClient.getNetworkInfo)
    assert(info.networkactive)
    assert(info.localrelay)
    assert(info.connections == 0)
    freshClient.addNode(otherFreshClient.getDaemon.uri, AddNodeArgument.Add)
    TestUtil.awaitConnection(freshClient, otherFreshClient)
    val newInfo = await(freshClient.getNetworkInfo)
    assert(newInfo.connections == 1)

  }

  it should "be able to get network statistics" in {
    TestUtil.createNodePair() flatMap {
      case (connectedClient, _) =>
        connectedClient.getNetTotals flatMap { stats => {
          assert(stats.timemillis.toBigInt > 0)
          assert(stats.totalbytesrecv > 0)
          assert(stats.totalbytessent > 0)
        }
        } flatMap { _ => succeed }
    }
  }

  it should "be able to ban and clear the ban of a subnet" in {
    val loopBack = URI.create("http://127.0.0.1")
    TestUtil.createNodePair().flatMap {
      case (client1, client2) =>
        client1.setBan(loopBack, "add").flatMap { _ =>
          client1.listBanned.flatMap { list =>
            assert(list.length == 1)
            assert(list.head.address.getAuthority == loopBack.getAuthority)
            assert(
              list.head.banned_until - list.head.ban_created == UInt32(86400))

            client1.setBan(loopBack, "remove").flatMap { _ =>
              client1.listBanned.flatMap { newList =>
                TestUtil.deleteNodePair(client1, client2)
                assert(newList.isEmpty)
              }
            }
          }
        }
    }
  }

  it should "be able to get the difficulty on the network" in {
    client.getDifficulty.map { difficulty =>
      assert(difficulty > 0)
      assert(difficulty < 1)
    }
  }

  it should "be able to deactivate and activate the network" in {
    client.setNetworkActive(false).flatMap { _ =>
      client.getNetworkInfo.flatMap { info =>
        assert(!info.networkactive)
        client.setNetworkActive(true).flatMap { _ =>
          client.getNetworkInfo.map { newInfo =>
            assert(newInfo.networkactive)
          }
        }
      }
    }
  }

  it should "be able to clear banned subnets" in {
    TestUtil.createNodePair().flatMap {
      case (client1, client2) =>
        client1.setBan(URI.create("http://127.0.0.1"), "add").flatMap { _ =>
          client1.setBan(URI.create("http://127.0.0.2"), "add").flatMap { _ =>
            client1.listBanned.flatMap { list =>
              assert(list.length == 2)

              client1.clearBanned().flatMap { _ =>
                client1.listBanned.flatMap { newList =>
                  TestUtil.deleteNodePair(client1, client2)
                  assert(newList.isEmpty)
                }
              }
            }
          }
        }
    }
  }
}
