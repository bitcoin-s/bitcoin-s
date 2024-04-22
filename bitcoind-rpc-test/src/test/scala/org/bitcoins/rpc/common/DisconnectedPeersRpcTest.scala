package org.bitcoins.rpc.common

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.{
  AddNodeArgument,
  SetBanCommand
}
import org.bitcoins.core.number.UInt32
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesCachedPairNewestDisconnected,
  BitcoindRpcTestUtil
}

import java.net.URI
import scala.concurrent.duration.DurationInt

class DisconnectedPeersRpcTest
    extends BitcoindFixturesCachedPairNewestDisconnected {
  behavior of "DisconnectedPeersRpcTest"

  it should "be able to ban and clear the ban of a subnet" in { case nodePair =>
    val client1 = nodePair.node1
    val loopBack = URI.create("http://127.0.0.1")
    for {
      _ <- client1.setBan(loopBack, SetBanCommand.Add)
      list <- client1.listBanned
      _ <- client1.setBan(loopBack, SetBanCommand.Remove)
      newList <- client1.listBanned
    } yield {
      assert(list.length == 1)
      assert(list.head.address.getAuthority == loopBack.getAuthority)
      assert(list.head.banned_until - list.head.ban_created == UInt32(86400))
      assert(newList.isEmpty)
    }
  }

  it should "be able to add and remove a node" in { case nodePair =>
    val freshClient = nodePair.node1
    val otherFreshClient = nodePair.node2
    val uri = otherFreshClient.getDaemon.uri
    for {
      _ <- freshClient.addNode(uri, AddNodeArgument.OneTry)
      _ <- BitcoindRpcTestUtil.awaitConnection(from = freshClient,
                                               to = otherFreshClient,
                                               interval = 1.second)

      info <- freshClient.getAddedNodeInfo(otherFreshClient.getDaemon.uri)

      _ <- freshClient.addNode(uri, AddNodeArgument.Remove)
      newInfo <- otherFreshClient.getAddedNodeInfo
    } yield {
      assert(info.length == 1)
      assert(info.head.addednode == otherFreshClient.getDaemon.uri)
      assert(info.head.connected.contains(true))
      assert(newInfo.isEmpty)
    }
  }

  it should "be able to add and disconnect a node" in { case nodePair =>
    val freshClient = nodePair.node1
    val otherFreshClient = nodePair.node2
    val uri = otherFreshClient.getDaemon.uri
    for {
      _ <- freshClient.addNode(uri, AddNodeArgument.OneTry)
      _ <- BitcoindRpcTestUtil.awaitConnection(freshClient,
                                               otherFreshClient,
                                               1.second)
      info <- freshClient.getAddedNodeInfo(otherFreshClient.getDaemon.uri)

      _ <- freshClient.disconnectNode(otherFreshClient.getDaemon.uri)
      _ <- BitcoindRpcTestUtil.awaitDisconnected(freshClient, otherFreshClient)
      newInfo <- freshClient.getAddedNodeInfo(otherFreshClient.getDaemon.uri)
    } yield {
      assert(info.head.connected.contains(true))
      assert(newInfo.head.connected.contains(false))
    }
  }

  it should "be able to get the connection count" in { case nodePair =>
    val freshClient = nodePair.node1
    val otherFreshClient = nodePair.node2
    for {
      connectionPre <- freshClient.getConnectionCount
      _ <-
        freshClient.addNode(otherFreshClient.getDaemon.uri,
                            AddNodeArgument.OneTry)
      _ <- BitcoindRpcTestUtil.awaitConnection(freshClient, otherFreshClient)
      connectionPost <- otherFreshClient.getConnectionCount
    } yield {
      assert(connectionPre == 0)
      assert(connectionPost == 1)
    }
  }

  it should "be able to submit a new block" in { case nodePair =>
    val client1 = nodePair.node1
    val client2 = nodePair.node2
    for {
      hash <- client2.generate(1)
      block <- client2.getBlockRaw(hash.head)
      preCount1 <- client1.getBlockCount()
      preCount2 <- client2.getBlockCount()
      _ <- client1.submitBlock(block)

      postCount1 <- client1.getBlockCount()
      postCount2 <- client2.getBlockCount()
      hash1 <- client1.getBlockHash(postCount1)
      hash2 <- client2.getBlockHash(postCount2)
    } yield {
      assert(preCount1 != preCount2)
      assert(postCount1 == postCount2)
      assert(hash1 == hash2)
    }
  }

  it should "be able to mark a block as precious" in { nodePair =>
    val freshClient = nodePair.node1
    val otherFreshClient = nodePair.node2

    for {
      blocks1 <-
        freshClient.generate(1)
      blocks2 <- otherFreshClient.generate(1)

      bestHash1 <- freshClient.getBestBlockHash()
      _ = assert(bestHash1 == blocks1.head)
      bestHash2 <- otherFreshClient.getBestBlockHash()
      _ = assert(bestHash2 == blocks2.head)

      _ <-
        freshClient
          .addNode(otherFreshClient.getDaemon.uri, AddNodeArgument.OneTry)
      _ <- AsyncUtil.retryUntilSatisfiedF(() =>
        BitcoindRpcTestUtil.hasSeenBlock(otherFreshClient, bestHash1))

      _ <- otherFreshClient.preciousBlock(bestHash1)
      newBestHash <- otherFreshClient.getBestBlockHash()

    } yield assert(newBestHash == bestHash1)
  }
}
