package org.bitcoins.rpc.common

import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.AddressType
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest

import scala.concurrent.Future

class MessageRpcTest extends BitcoindRpcTest {

  val clientF: Future[BitcoindRpcClient] =
    BitcoindRpcTestUtil.startedBitcoindRpcClient().map { client =>
      clientAccum += client
      client
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
