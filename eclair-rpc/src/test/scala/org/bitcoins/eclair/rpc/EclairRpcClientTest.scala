package org.bitcoins.eclair.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.protocol.ln.channel.ChannelId
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.eclair.rpc.client.EclairRpcClient
import org.bitcoins.eclair.rpc.json.ChannelResult
import org.bitcoins.rpc.RpcUtil
import org.bitcoins.rpc.client.BitcoindRpcClient
import org.scalatest.{ Assertion, AsyncFlatSpec, BeforeAndAfterAll }

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.DurationInt

class EclairRpcClientTest extends AsyncFlatSpec with BeforeAndAfterAll {

  implicit val system = ActorSystem("EclairRpcClient")
  implicit val m = ActorMaterializer.create(system)
  implicit val ec = m.executionContext
  implicit val bitcoinNp = EclairTestUtil.bitcoinNetwork

  val logger = BitcoinSLogger.logger

  val bitcoindInstance = EclairTestUtil.startedBitcoindInstance()
  val bitcoindRpc = new BitcoindRpcClient(bitcoindInstance)

  val e1Instance = EclairTestUtil.eclairInstance(bitcoindInstance)
  val e2Instance = EclairTestUtil.eclairInstance(bitcoindInstance)

  val client = new EclairRpcClient(e1Instance)
  val otherClient = new EclairRpcClient(e2Instance)

  override def beforeAll(): Unit = {
    logger.info(s"Temp eclair directory created ${client.getDaemon.authCredentials.datadir}")
    logger.info(s"Temp eclair directory created ${otherClient.getDaemon.authCredentials.datadir}")

    client.start()
    otherClient.start()

    RpcUtil.awaitCondition(
      condition = client.isStarted,
      duration = 1.second)

    RpcUtil.awaitCondition(
      condition = otherClient.isStarted,
      duration = 1.second)

    logger.debug(s"Both clients started")

    val infoF = otherClient.getInfo

    val connection: Future[String] = infoF.flatMap { info =>
      client.connect(info.nodeId, "localhost", info.port)
    }

    def isConnected(): Future[Boolean] = {
      val nodeIdF = infoF.map(_.nodeId)
      nodeIdF.flatMap { nodeId =>
        connection.flatMap { _ =>
          val connected: Future[Boolean] = client.isConnected(nodeId)
          connected
        }
      }
    }

    logger.debug(s"Awaiting connection between clients")
    RpcUtil.awaitConditionF(
      conditionF = isConnected,
      duration = 1.second)
  }

  behavior of "RpcClient"

  it should "be able to open a channel" in {
    val result: Future[Assertion] = {
      otherClient.getInfo.flatMap { info =>
        val openedChanF = client.open(info.nodeId, 100000)
        openedChanF.flatMap(channelId => hasChannel(client, channelId))
      }
    }

    result
  }

  /** Checks that the given [[org.bitcoins.eclair.rpc.client.EclairRpcClient]] has the given chanId */
  private def hasChannel(client: EclairRpcClient, chanId: ChannelId): Future[Assertion] = {
    val recognizedOpenChannel: Future[Assertion] = {
      val chanResultF: Future[ChannelResult] = client.channel(chanId)
      chanResultF.map(c => assert(c.channelId == chanId))
    }

    recognizedOpenChannel
  }

  override def afterAll(): Unit = {
    val s1 = client.stop()
    val s2 = otherClient.stop()
    val s3 = bitcoindRpc.stop()
    Await.result(system.terminate(), 10.seconds)
  }
}
