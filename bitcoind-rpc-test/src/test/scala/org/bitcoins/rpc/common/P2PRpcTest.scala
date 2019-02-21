package org.bitcoins.rpc.common

import java.net.URI

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.number.UInt32
import org.bitcoins.rpc.{BitcoindRpcTestConfig, BitcoindRpcTestUtil}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.AddNodeArgument
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.async.Async.{async, await}
import scala.concurrent.{Await, ExecutionContext}

class P2PRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("P2PRpcTest")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  val client = new BitcoindRpcClient(BitcoindRpcTestUtil.instance())

  override protected def beforeAll(): Unit = {
    import BitcoindRpcTestConfig.DEFAULT_TIMEOUT
    Await.result(BitcoindRpcTestUtil.startServers(Vector(client)),
                 DEFAULT_TIMEOUT)
  }

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(Vector(client))
    TestKit.shutdownActorSystem(system)
  }

  behavior of "P2PRpcTest"

  it should "be able to get peer info" in async {
    val (freshClient, otherFreshClient) =
      await(BitcoindRpcTestUtil.createNodePair())
    val infoList = await(freshClient.getPeerInfo)
    assert(infoList.length == 1)

    val info = infoList.head
    assert(!info.inbound)
    assert(info.addnode)
    assert(info.networkInfo.addr == otherFreshClient.getDaemon.uri)
    BitcoindRpcTestUtil.deleteNodePair(freshClient, otherFreshClient)
    succeed
  }

  it should "be able to get the added node info" in async {
    val (freshClient, otherFreshClient) =
      await(BitcoindRpcTestUtil.createNodePair())
    val info = await(freshClient.getAddedNodeInfo)

    assert(info.length == 1)
    assert(info.head.addednode == otherFreshClient.getDaemon.uri)
    assert(info.head.connected.contains(true))

    BitcoindRpcTestUtil.deleteNodePair(freshClient, otherFreshClient)
    succeed

  }

  it should "be able to get the network info" in async {
    val (freshClient, otherFreshClient) =
      await(BitcoindRpcTestUtil.createUnconnectedNodePair())
    val info = await(freshClient.getNetworkInfo)
    assert(info.networkactive)
    assert(info.localrelay)
    assert(info.connections == 0)
    freshClient.addNode(otherFreshClient.getDaemon.uri, AddNodeArgument.Add)
    BitcoindRpcTestUtil.awaitConnection(freshClient, otherFreshClient)
    val newInfo = await(freshClient.getNetworkInfo)
    assert(newInfo.connections == 1)

    BitcoindRpcTestUtil.deleteNodePair(freshClient, otherFreshClient)
    succeed

  }

  it should "be able to get network statistics" in async {
    val (connectedClient, otherClient) =
      await(BitcoindRpcTestUtil.createNodePair())
    val stats = await(connectedClient.getNetTotals)

    assert(stats.timemillis.toBigInt > 0)
    assert(stats.totalbytesrecv > 0)
    assert(stats.totalbytessent > 0)

    BitcoindRpcTestUtil.deleteNodePair(connectedClient, otherClient)
    succeed
  }

  it should "be able to ban and clear the ban of a subnet" in async {
    val loopBack = URI.create("http://127.0.0.1")
    val (client1, client2) = await(BitcoindRpcTestUtil.createNodePair())
    await(client1.setBan(loopBack, "add"))

    val list = await(client1.listBanned)
    assert(list.length == 1)
    assert(list.head.address.getAuthority == loopBack.getAuthority)
    assert(list.head.banned_until - list.head.ban_created == UInt32(86400))

    await(client1.setBan(loopBack, "remove"))
    val newList = await(client1.listBanned)
    assert(newList.isEmpty)

    BitcoindRpcTestUtil.deleteNodePair(client1, client2)
    succeed
  }

  it should "be able to get the difficulty on the network" in {
    client.getDifficulty.map { difficulty =>
      assert(difficulty > 0)
      assert(difficulty < 1)
    }
  }

  it should "be able to deactivate and activate the network" in async {
    val infoF = for {
      _ <- client.setNetworkActive(false)
      firstInfo <- client.getNetworkInfo
      _ <- client.setNetworkActive(true)
      secondInfo <- client.getNetworkInfo
    } yield (firstInfo, secondInfo)

    val (firstInfo, secondInfo) = await(infoF)
    assert(!firstInfo.networkactive)
    assert(secondInfo.networkactive)
  }

  it should "be able to clear banned subnets" in async {
    val (client1, client2) = await(BitcoindRpcTestUtil.createNodePair())

    await(client1.setBan(URI.create("http://127.0.0.1"), "add"))
    await(client1.setBan(URI.create("http://127.0.0.2"), "add"))

    val list = await(client1.listBanned)
    assert(list.length == 2)

    await(client1.clearBanned())
    val newList = await(client1.listBanned)
    assert(newList.isEmpty)

    BitcoindRpcTestUtil.deleteNodePair(client1, client2)
    succeed
  }

  it should "be able to add and remove a node" in async {
    val (freshClient, otherFreshClient) =
      await(BitcoindRpcTestUtil.createUnconnectedNodePair())
    val uri = otherFreshClient.getDaemon.uri

    await(freshClient.addNode(uri, AddNodeArgument.Add))

    BitcoindRpcTestUtil.awaitConnection(freshClient, otherFreshClient)

    val info =
      await(freshClient.getAddedNodeInfo(otherFreshClient.getDaemon.uri))
    assert(info.length == 1)
    assert(info.head.addednode == otherFreshClient.getDaemon.uri)
    assert(info.head.connected.contains(true))

    await(freshClient.addNode(uri, AddNodeArgument.Remove))

    val newInfo = await(otherFreshClient.getAddedNodeInfo)
    assert(newInfo.isEmpty)

    BitcoindRpcTestUtil.deleteNodePair(freshClient, otherFreshClient)
    succeed
  }

  it should "be able to add and disconnect a node" in async {
    val (freshClient, otherFreshClient) =
      await(BitcoindRpcTestUtil.createUnconnectedNodePair())
    val uri = otherFreshClient.getDaemon.uri

    val addNodeF = freshClient.addNode(uri, AddNodeArgument.Add)
    await(addNodeF)
    BitcoindRpcTestUtil.awaitConnection(freshClient, otherFreshClient)

    val info =
      await(freshClient.getAddedNodeInfo(otherFreshClient.getDaemon.uri))
    assert(info.head.connected.contains(true))

    await(freshClient.disconnectNode(otherFreshClient.getDaemon.uri))
    BitcoindRpcTestUtil.awaitDisconnected(freshClient, otherFreshClient)

    val newInfoF = freshClient.getAddedNodeInfo(otherFreshClient.getDaemon.uri)
    val newInfo = await(newInfoF)
    assert(newInfo.head.connected.contains(false))

    BitcoindRpcTestUtil.deleteNodePair(freshClient, otherFreshClient)
    succeed
  }

  it should "be able to get the connection count" in async {
    val (freshClient, otherFreshClient) =
      await(BitcoindRpcTestUtil.createUnconnectedNodePair())
    val connectionPre = await(freshClient.getConnectionCount)
    assert(connectionPre == 0)

    await(
      freshClient.addNode(otherFreshClient.getDaemon.uri, AddNodeArgument.Add))
    BitcoindRpcTestUtil.awaitConnection(freshClient, otherFreshClient)

    val connectionPost = await(otherFreshClient.getConnectionCount)
    assert(connectionPost == 1)

    BitcoindRpcTestUtil.deleteNodePair(freshClient, otherFreshClient)
    succeed
  }

  it should "be able to submit a new block" in async {
    val (client1, client2) =
      await(BitcoindRpcTestUtil.createUnconnectedNodePair())

    val hash = await(client2.generate(1))
    val block = await(
      client2.getBlockRaw(hash.head)
    )

    val preCount1 = await(client1.getBlockCount)
    val preCount2 = await(client2.getBlockCount)
    assert(preCount1 != preCount2)

    await(client1.submitBlock(block))

    val postCount1 = await(client1.getBlockCount)
    val postCount2 = await(client2.getBlockCount)
    assert(postCount1 == postCount2)

    val hash1 = await(client1.getBlockHash(postCount1))
    val hash2 = await(client2.getBlockHash(postCount2))
    assert(hash1 == hash2)

    BitcoindRpcTestUtil.deleteNodePair(client1, client2)
    succeed
  }
}
