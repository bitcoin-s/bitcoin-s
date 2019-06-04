package org.bitcoins.eclair.rpc
import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.bitcoins.eclair.rpc.client.EclairRpcClient
import org.bitcoins.testkit.eclair.rpc.EclairRpcTestUtil
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}
import org.slf4j.LoggerFactory
import akka.stream.StreamTcpException

class EclairRpcTestUtilTest extends AsyncFlatSpec with BeforeAndAfterAll {

  private val logger = LoggerFactory.getLogger(getClass)

  implicit private val actorSystem: ActorSystem =
    ActorSystem("EclairRpcTestUtilTest", BitcoindRpcTestUtil.AKKA_CONFIG)

  private lazy val bitcoindRpcF = {
    val cliF = EclairRpcTestUtil.startedBitcoindRpcClient()
    val blocksF = cliF.flatMap(_.generate(200))
    blocksF.flatMap(_ => cliF)
  }

  private val clients =
    Vector.newBuilder[EclairRpcClient]

  override def afterAll: Unit = {
    clients.result().foreach(EclairRpcTestUtil.shutdown)
    TestKit.shutdownActorSystem(actorSystem)
  }

  behavior of "EclairRpcTestUtilTest"

  it must "spawn a V16 bitcoind instance" in {
    for {
      bitcoind <- EclairRpcTestUtil.startedBitcoindRpcClient()
      _ <- bitcoind.getNetworkInfo
      _ <- BitcoindRpcTestUtil.stopServer(bitcoind)
      _ <- bitcoind.getNetworkInfo
        .map(_ => fail("got info from stopped bitcoind!"))
        .recover { case _: StreamTcpException => }
    } yield succeed
  }

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
