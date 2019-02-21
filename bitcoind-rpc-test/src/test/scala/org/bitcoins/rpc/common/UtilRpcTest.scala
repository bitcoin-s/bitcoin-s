package org.bitcoins.rpc.common

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.rpc.BitcoindRpcTestUtil
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.AddressType
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.concurrent.{Await, ExecutionContext}

class UtilRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("UtilRpcTest")
  implicit val ec: ExecutionContext = system.getDispatcher
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  private val client = new BitcoindRpcClient(BitcoindRpcTestUtil.instance())

  private val otherClient = new BitcoindRpcClient(
    BitcoindRpcTestUtil.instance())

  override protected def beforeAll(): Unit = {
    import org.bitcoins.rpc.BitcoindRpcTestConfig.DEFAULT_TIMEOUT
    val startF = BitcoindRpcTestUtil.startServers(Vector(client, otherClient))
    Await.result(startF, DEFAULT_TIMEOUT)
  }

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(Vector(client, otherClient))
    TestKit.shutdownActorSystem(system)
  }

  behavior of "RpcUtilTest"

  it should "be able to validate a bitcoin address" in {
    otherClient.getNewAddress.flatMap { address =>
      client.validateAddress(address).map { validation =>
        assert(validation.isvalid)
      }
    }
  }

  it should "be able to decode a reedem script" in {
    val ecPrivKey1 = ECPrivateKey.freshPrivateKey
    val pubKey1 = ecPrivKey1.publicKey

    client.getNewAddress(addressType = AddressType.Legacy).flatMap { address =>
      client
        .addMultiSigAddress(2,
                            Vector(Left(pubKey1),
                                   Right(address.asInstanceOf[P2PKHAddress])))
        .flatMap { multisig =>
          client.decodeScript(multisig.redeemScript).map { decoded =>
            assert(decoded.reqSigs.contains(2))
            assert(decoded.typeOfScript.contains("multisig"))
            assert(decoded.addresses.get.contains(address))
          }
        }
    }
  }
}
