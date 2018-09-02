package org.bitcoins.eclair.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.eclair.rpc.client.RpcClient
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

class RpcClientTest extends AsyncFlatSpec with BeforeAndAfterAll  {
  implicit val system = ActorSystem("EclairRpcClient")
  implicit val m = ActorMaterializer.create(system)
  implicit val ec = m.executionContext
  implicit val networkParam = TestUtil.network

  val client = new RpcClient(TestUtil.instance())
  val otherClient = new RpcClient(TestUtil.instance())

  val logger = BitcoinSLogger.logger

  override def beforeAll(): Unit = {
    ???
  }

  behavior of "RpcClient"

  override def afterAll(): Unit = {
    logger.info(client.stop())
    logger.info(otherClient.stop())
    if (TestUtil.deleteTmpDir(client.getDaemon.authCredentials.datadir))
      logger.info("Temp bitcoin directory deleted")
    if (TestUtil.deleteTmpDir(otherClient.getDaemon.authCredentials.datadir))
      logger.info("Temp bitcoin directory deleted")
  }
}
