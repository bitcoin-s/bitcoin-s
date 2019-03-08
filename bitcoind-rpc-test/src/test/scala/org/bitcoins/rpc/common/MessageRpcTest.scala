package org.bitcoins.rpc.common

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.rpc.BitcoindRpcTestUtil
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.AddressType
import org.scalatest.{AsyncFlatSpec, BeforeAndAfter, BeforeAndAfterAll}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

class MessageRpcTest
    extends AsyncFlatSpec
    with BeforeAndAfterAll
    with BeforeAndAfter {
  implicit val system: ActorSystem =
    ActorSystem("MessageRpcTest_ActorSystem", BitcoindRpcTestUtil.AKKA_CONFIG)
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  val accum: mutable.Builder[BitcoindRpcClient, Vector[BitcoindRpcClient]] =
    Vector.newBuilder[BitcoindRpcClient]

  val clientF: Future[BitcoindRpcClient] =
    BitcoindRpcTestUtil.startedBitcoindRpcClient().map { client =>
      accum += client
      client
    }

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(accum.result)
    TestKit.shutdownActorSystem(system)
  }

  behavior of "MessageRpc"

  it should "be able to sign a message and verify that signature" in {
    val message = "Never gonna give you up\nNever gonna let you down\n..."
    for {
      client <- clientF
      address <- client.getNewAddress(addressType = AddressType.Legacy)
      signature <- client.signMessage(address.asInstanceOf[P2PKHAddress],
                                      message)
      validity <- client
        .verifyMessage(address.asInstanceOf[P2PKHAddress], signature, message)
    } yield assert(validity)
  }

  it should "be able to sign a message with a private key and verify that signature" in {
    val message = "Never gonna give you up\nNever gonna let you down\n..."
    val privKey = ECPrivateKey.freshPrivateKey
    val address = P2PKHAddress(privKey.publicKey, networkParam)

    for {
      client <- clientF
      signature <- client.signMessageWithPrivKey(privKey, message)
      validity <- client.verifyMessage(address, signature, message)
    } yield assert(validity)
  }
}
