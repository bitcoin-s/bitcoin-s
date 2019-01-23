package org.bitcoins.eclair.rpc
import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.bitcoins.eclair.rpc.client.EclairRpcClient
import org.bitcoins.rpc.BitcoindRpcTestUtil
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

class EclairRpcTestUtilTest extends AsyncFlatSpec with BeforeAndAfterAll {

  private implicit val actorSystem: ActorSystem =
    ActorSystem.create("EclairRpcTestUtilTest")

  private val bitcoindRpc = BitcoindRpcTestUtil.startedBitcoindRpcClient()
  private val clients =
    Vector.newBuilder[EclairRpcClient]

  override def beforeAll: Unit = {
    bitcoindRpc.generate(200)
  }

  override def afterAll: Unit = {
    clients.result().foreach(EclairRpcTestUtil.shutdown)
    TestKit.shutdownActorSystem(actorSystem)
  }

  behavior of "EclairRpcTestUtilTest"

  it must "spawn four nodes and create a P2P link between them" in {
    val EclairNodes4(first, second, third, fourth) =
      EclairRpcTestUtil.createNodeLink(bitcoindRpc)
    clients ++= List(first, second, third, fourth)

    for {
      nodeInfoFirst <- first.getInfo
      peersFirst <- first.getPeers
      nodeInfoSecond <- second.getInfo
      peersSecond <- second.getPeers
      nodeInfoThird <- third.getInfo
      peersThird <- third.getPeers
      nodeInfoFourth <- fourth.getInfo
      peersFourth <- fourth.getPeers
    } yield {
      assert(peersFirst.length == 1)
      assert(peersFirst.exists(_.nodeId == nodeInfoSecond.nodeId))

      assert(peersSecond.length == 2)
      assert(peersSecond.exists(_.nodeId == nodeInfoFirst.nodeId))
      assert(peersSecond.exists(_.nodeId == nodeInfoThird.nodeId))

      assert(peersThird.length == 2)
      assert(peersThird.exists(_.nodeId == nodeInfoSecond.nodeId))
      assert(peersThird.exists(_.nodeId == nodeInfoFourth.nodeId))

      assert(peersFourth.length == 1)
      assert(peersFourth.exists(_.nodeId == nodeInfoThird.nodeId))
    }
  }

  it must "spawn four nodes and create a channel link between them" in {
    val EclairNodes4(first, second, third, fourth) =
      EclairRpcTestUtil.createNodeLink(bitcoindRpc)
    clients ++= List(first, second, third, fourth)

    for {
      nodeInfoFirst <- first.getInfo
      channelsFirst <- first.channels
      nodeInfoSecond <- second.getInfo
      channelsSecond <- second.channels
      nodeInfoThird <- third.getInfo
      channelsThird <- third.channels
      nodeInfoFourth <- fourth.getInfo
      channelsFourth <- fourth.channels
    } yield {
      assert(channelsFirst.length == 1)
      assert(channelsFirst.exists(_.nodeId == nodeInfoSecond.nodeId))

      assert(channelsSecond.length == 2)
      assert(channelsSecond.exists(_.nodeId == nodeInfoFirst.nodeId))
      assert(channelsSecond.exists(_.nodeId == nodeInfoThird.nodeId))

      assert(channelsThird.length == 2)
      assert(channelsThird.exists(_.nodeId == nodeInfoSecond.nodeId))
      assert(channelsThird.exists(_.nodeId == nodeInfoFourth.nodeId))

      assert(channelsFourth.length == 1)
      assert(channelsFourth.exists(_.nodeId == nodeInfoThird.nodeId))
    }
  }
}
