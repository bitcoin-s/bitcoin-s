package org.bitcoins.eclair.rpc
import akka.actor.ActorSystem
import org.bitcoins.eclair.rpc.client.EclairRpcClient
import org.bitcoins.testkit.eclair.rpc.EclairRpcTestUtil
import org.bitcoins.testkit.util.BitcoinSAsyncTest

class EclairRpcTestUtilTest extends BitcoinSAsyncTest {

  implicit private val actorSystem: ActorSystem =
    ActorSystem("EclairRpcTestUtilTest")

  private lazy val bitcoindRpcF =
    for {
      cli <- EclairRpcTestUtil.startedBitcoindRpcClient()
      address <- cli.getNewAddress
      blocks <- cli.generateToAddress(200, address)
    } yield cli

  private val clients =
    Vector.newBuilder[EclairRpcClient]

  override def afterAll: Unit = {
    clients.result().foreach(EclairRpcTestUtil.shutdown)
    super.afterAll()
  }

  behavior of "EclairRpcTestUtilTest"

  it must "spawn four nodes and create a channel link between them" in {
    val nodes4F = bitcoindRpcF.flatMap { bitcoindRpc =>
      val nodes = EclairRpcTestUtil.createNodeLink(bitcoindRpc)

      nodes.map { n4 =>
        clients ++= List(n4.c1, n4.c2, n4.c3, n4.c4)
      }

      nodes
    }

    nodes4F.flatMap { n4 =>
      val first = n4.c1
      val second = n4.c2
      val third = n4.c3
      val fourth = n4.c4

      for {
        nodeInfoFirst <- first.getInfo
        channelsFirst <- first.channels()
        nodeInfoSecond <- second.getInfo
        channelsSecond <- second.channels()
        nodeInfoThird <- third.getInfo
        channelsThird <- third.channels()
        nodeInfoFourth <- fourth.getInfo
        channelsFourth <- fourth.channels()
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
}
