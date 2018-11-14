package org.bitcoins.rpc.common

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.BitcoindRpcTestUtil
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.AddressType
import org.scalatest.{ AsyncFlatSpec, BeforeAndAfter, BeforeAndAfterAll }
import org.slf4j.Logger

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, ExecutionContext }

class MessageRpcTest extends AsyncFlatSpec with BeforeAndAfterAll with BeforeAndAfter {
  implicit val system: ActorSystem = ActorSystem("MessageRpcTest_ActorSystem")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  val client = new BitcoindRpcClient(BitcoindRpcTestUtil.instance())
  val clients = Vector(client)

  val logger: Logger = BitcoinSLogger.logger

  override protected def beforeAll(): Unit = {
    logger.info("Starting MessageRpcTest")
    logger.info("Bitcoin server starting")
    BitcoindRpcTestUtil.startServers(clients)
    logger.info("Bitcoin server started")
  }

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(clients)
    Await.result(system.terminate(), 10.seconds)
  }

  behavior of "MessageRpc"

  it should "be able to sign a message and verify that signature" in {
    val message = "Never gonna give you up\nNever gonna let you down\n..."
    client.getNewAddress(addressType = AddressType.Legacy).flatMap { address =>
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
