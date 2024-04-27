package org.bitcoins.rpc.common

import org.bitcoins.commons.jsonmodels.bitcoind.GetNodeAddressesResultPostV22
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.SetBanCommand
import org.bitcoins.testkit.rpc.BitcoindFixturesCachedPairNewest

import java.net.URI
import scala.concurrent.Future

class P2PRpcTest extends BitcoindFixturesCachedPairNewest {

  behavior of "P2PRpcTest"

  it should "be able to get the added node info" in { case nodePair =>
    val (freshClient, otherFreshClient) = (nodePair.node1, nodePair.node2)
    for {
      info <- freshClient.getAddedNodeInfo
    } yield {
      assert(info.length == 1)
      assert(info.head.addednode == otherFreshClient.getDaemon.uri)
      assert(info.head.connected.contains(true))
    }
  }

  it should "be able to get the network info" in { case nodePair =>
    val freshClient = nodePair.node1
    for {
      info <- freshClient.getNetworkInfo
    } yield {
      assert(info.networkactive)
      assert(info.localrelay)
      assert(info.connections == 1)
    }

  }

  it should "be able to get network statistics" in { case nodePair =>
    val connectedClient = nodePair.node1
    for {
      stats <- connectedClient.getNetTotals
    } yield {
      assert(stats.timemillis.toBigInt > 0)
      assert(stats.totalbytesrecv > 0)
      assert(stats.totalbytessent > 0)

    }
  }

  it should "be able to get the difficulty on the network" in { case nodePair =>
    val client = nodePair.node1
    for {
      difficulty <- client.getDifficulty
    } yield {
      assert(difficulty > 0)
      assert(difficulty < 1)
    }
  }

  it should "be able to deactivate and activate the network" in {
    case nodePair =>
      val client = nodePair.node1
      for {
        _ <- client.setNetworkActive(false)
        firstInfo <- client.getNetworkInfo
        _ <- client.setNetworkActive(true)
        secondInfo <- client.getNetworkInfo
      } yield {
        assert(!firstInfo.networkactive)
        assert(secondInfo.networkactive)
      }
  }

  it should "be able to clear banned subnets" in { case nodePair =>
    val client1 = nodePair.node1
    for {
      _ <- client1.setBan(URI.create("http://127.0.0.1"), SetBanCommand.Add)
      _ <- client1.setBan(URI.create("http://127.0.0.2"), SetBanCommand.Add)
      list <- client1.listBanned
      _ <- client1.clearBanned()
      newList <- client1.listBanned
    } yield {
      assert(list.length == 2)
      assert(newList.isEmpty)
    }
  }

  it should "take a network input and output addresses of same network" in {
    case nodePair =>
      val networkOption = Vector("ipv4", "ipv6", "onion", "i2p")
      val client = nodePair.node1
      for {
        _ <- Future.traverse(networkOption) { networkType =>
          val resultVecF: Future[Vector[GetNodeAddressesResultPostV22]] =
            client.getNodeAddresses(networkType, 10)
          resultVecF.map { resultVec =>
            resultVec.map { result =>
              assert(result.network == networkType)
            }
          }
        }
      } yield {
        succeed
      }
  }

  it should "return a network address" in { case nodePair =>
    val client = nodePair.node1
    for {
      resultVec <- client.getNodeAddresses()
    } yield {
      resultVec.foreach { result =>
        assert(
          result.network == "ipv4" || result.network == "ipv6" || result.network == "onion" || result.network == "i2p"
        )
      }
      succeed
    }
  }

  it should "getaddrmaninfo" in { case nodePair =>
    val client = nodePair.node1
    for {
      result <- client.getAddrManInfo()
    } yield {
      assert(result.ipv4.total == 0)
      assert(result.ipv6.total == 0)
      assert(result.onion.total == 0)
      assert(result.cjdns.total == 0)
      assert(result.all_networks.total == 0)
    }
  }
}
