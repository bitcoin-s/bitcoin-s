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

class MultisigRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem =
    ActorSystem("MultisigRpcTest", BitcoindRpcTestUtil.AKKA_CONFIG)
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  val clientAccum: mutable.Builder[
    BitcoindRpcClient,
    Vector[BitcoindRpcClient]] = Vector.newBuilder[BitcoindRpcClient]

  lazy val clientF: Future[BitcoindRpcClient] =
    BitcoindRpcTestUtil.startedBitcoindRpcClient(clientAccum = clientAccum)

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(clientAccum.result)
    TestKit.shutdownActorSystem(system)
  }

  behavior of "MultisigRpc"

  it should "be able to create a multi sig address" in {
    val ecPrivKey1 = ECPrivateKey.freshPrivateKey
    val ecPrivKey2 = ECPrivateKey.freshPrivateKey

    val pubKey1 = ecPrivKey1.publicKey
    val pubKey2 = ecPrivKey2.publicKey

    for {
      client <- clientF
      _ <- client.createMultiSig(2, Vector(pubKey1, pubKey2))
    } yield succeed
  }

  it should "be able to add a multi sig address to the wallet" in {
    val ecPrivKey1 = ECPrivateKey.freshPrivateKey
    val pubKey1 = ecPrivKey1.publicKey

    for {
      client <- clientF
      address <- client.getNewAddress(addressType = AddressType.Legacy)
      _ <- {
        val pubkey = Left(pubKey1)
        val p2pkh = Right(address.asInstanceOf[P2PKHAddress])
        client
          .addMultiSigAddress(2, Vector(pubkey, p2pkh))
      }
    } yield succeed
  }

}
