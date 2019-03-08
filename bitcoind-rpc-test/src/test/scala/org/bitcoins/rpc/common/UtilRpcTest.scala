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

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

class UtilRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem =
    ActorSystem("UtilRpcTest", BitcoindRpcTestUtil.AKKA_CONFIG)
  implicit val ec: ExecutionContext = system.getDispatcher
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  val accum: mutable.Builder[BitcoindRpcClient, Vector[BitcoindRpcClient]] =
    Vector.newBuilder[BitcoindRpcClient]

  lazy val clientsF: Future[(BitcoindRpcClient, BitcoindRpcClient)] =
    BitcoindRpcTestUtil.createNodePair(clientAccum = accum)

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(accum.result)
    TestKit.shutdownActorSystem(system)
  }

  behavior of "RpcUtilTest"

  it should "be able to validate a bitcoin address" in {
    for {
      (client, otherClient) <- clientsF
      address <- otherClient.getNewAddress
      validation <- client.validateAddress(address)
    } yield assert(validation.isvalid)
  }

  it should "be able to decode a reedem script" in {
    val ecPrivKey1 = ECPrivateKey.freshPrivateKey
    val pubKey1 = ecPrivKey1.publicKey
    for {
      (client, _) <- clientsF
      address <- client.getNewAddress(addressType = AddressType.Legacy)
      multisig <- client
        .addMultiSigAddress(
          2,
          Vector(Left(pubKey1), Right(address.asInstanceOf[P2PKHAddress])))
      decoded <- client.decodeScript(multisig.redeemScript)
    } yield {
      assert(decoded.reqSigs.contains(2))
      assert(decoded.typeOfScript.contains("multisig"))
      assert(decoded.addresses.get.contains(address))
    }
  }
}
