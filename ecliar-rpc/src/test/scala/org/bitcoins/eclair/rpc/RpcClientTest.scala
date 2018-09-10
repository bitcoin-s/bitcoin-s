package org.bitcoins.eclair.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.eclair.rpc.client.RpcClient
import org.scalatest.{ AsyncFlatSpec, BeforeAndAfterAll }

class RpcClientTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system = ActorSystem("EclairRpcClient")
  implicit val m = ActorMaterializer.create(system)
  implicit val ec = m.executionContext
  implicit val networkParam = TestUtil.network

  val client = new RpcClient(TestUtil.instance())
  val otherClient = new RpcClient(TestUtil.instance())

  val logger = BitcoinSLogger.logger

  override def beforeAll(): Unit = {
    logger.info("Temp eclair directory created")
    logger.info("Temp eclair directory created")

    logger.debug(client.getDaemon.authCredentials.datadir.toString)

    client.start()
    otherClient.start()

    otherClient.getInfo.map { info =>
      client.connect(info.nodeId, "localhost", info.port)
    }

    Thread.sleep(2000)
  }

  behavior of "RpcClient"

  it should "be able to open a channel" in {
    otherClient.getInfo.flatMap { info =>
      client.open(info.nodeId, 100000).map { opened =>
        assert(!opened.contains("command failed"))
      }
    }
  }

  override def afterAll(): Unit = {
    logger.info(client.stop())
    logger.info(otherClient.stop())
    if (TestUtil.deleteTmpDir(client.getDaemon.authCredentials.datadir))
      logger.info("Temp eclair directory deleted")
    if (TestUtil.deleteTmpDir(otherClient.getDaemon.authCredentials.datadir))
      logger.info("Temp eclair directory deleted")
  }
}
