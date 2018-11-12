package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.AddressType
import org.scalatest.{ AsyncFlatSpec, BeforeAndAfterAll }
import org.slf4j.Logger

import scala.async.Async.{ async, await }
import scala.concurrent.ExecutionContext

class MultisigRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("MultisigRpcTest_ActorSystem")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = TestUtil.network

  val client = new BitcoindRpcClient(TestUtil.instance())

  val logger: Logger = BitcoinSLogger.logger

  override protected def beforeAll(): Unit = {
    logger.info("Starting MultisigRpcTest")
    logger.info("Bitcoin server starting")
    TestUtil.startServers(client)
    logger.info("Bitcoin server started")
  }

  override protected def afterAll(): Unit = async {
    logger.info("Cleaning up after MultisigRpcTest")
    logger.info("Stopping Bitcoin server")
    TestUtil.stopServers(client)
    logger.info("Bitcoin server stopped")

    logger.info("Stopping ActorSystem")
    await(system.terminate)
    logger.info("Stopped ActorSystem")

  }

  behavior of "MultisigRpc"

  it should "be able to create a multi sig address" in async {
    val ecPrivKey1 = ECPrivateKey.freshPrivateKey
    val ecPrivKey2 = ECPrivateKey.freshPrivateKey

    val pubKey1 = ecPrivKey1.publicKey
    val pubKey2 = ecPrivKey2.publicKey

    client.createMultiSig(2, Vector(pubKey1, pubKey2)).map { _ =>
      succeed
    }
    succeed
  }

  it should "be able to add a multi sig address to the wallet" in async {
    val ecPrivKey1 = ECPrivateKey.freshPrivateKey
    val pubKey1 = ecPrivKey1.publicKey
    val address = await(client.getNewAddress(addressType = AddressType.Legacy))

    await(client
      .addMultiSigAddress(
        2,
        Vector(
          Left(pubKey1),
          Right(address.asInstanceOf[P2PKHAddress]))))

    succeed
  }
}
