package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.{ BitcoindRpcClient, RpcOpts }
import org.scalatest.{ AsyncFlatSpec, BeforeAndAfter, BeforeAndAfterAll }
import org.slf4j.Logger

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, ExecutionContext }

class MessageRpcTest extends AsyncFlatSpec with BeforeAndAfterAll with BeforeAndAfter {
  implicit val system: ActorSystem = ActorSystem("MessageRpcTest_ActorSystem")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = TestUtil.network

  val client = new BitcoindRpcClient(TestUtil.instance())

  val logger: Logger = BitcoinSLogger.logger

  override protected def beforeAll(): Unit = {
    logger.info("Starting MessageRpcTest")
    logger.info("Bitcoin server starting")
    TestUtil.startServers(client)
    logger.info("Bitcoin server started")
  }

  override protected def afterAll(): Unit = {
    logger.info("Cleaning up after MessageRpcTest")
    logger.info("Stopping Bitcoin server")
    TestUtil.stopServers(client)
    logger.info("Bitcoin server stopped")

    logger.info("Stopping ActorSystem")
    Await.result(system.terminate(), 10.seconds)
    logger.info("Stopped ActorSystem")

  }

  behavior of "MessageRpc"

  it should "be able to sign a message and verify that signature" in {
    val message = "Never gonna give you up\nNever gonna let you down\n..."
    client.getNewAddress(addressType = RpcOpts.Legacy()).flatMap { address =>
      client.signMessage(address.asInstanceOf[P2PKHAddress], message).flatMap {
        signature =>
          client
            .verifyMessage(
              address.asInstanceOf[P2PKHAddress],
              signature,
              message)
            .map { validity =>
              assert(validity)
            }
      }
    }
  }

  it should "be able to sign a message with a private key and verify that signature" in {
    val message = "Never gonna give you up\nNever gonna let you down\n..."
    val privKey = ECPrivateKey.freshPrivateKey
    val address = P2PKHAddress(privKey.publicKey, networkParam)

    client.signMessageWithPrivKey(privKey, message).flatMap { signature =>
      client.verifyMessage(address, signature, message).map { validity =>
        assert(validity)
      }
    }
  }
}
