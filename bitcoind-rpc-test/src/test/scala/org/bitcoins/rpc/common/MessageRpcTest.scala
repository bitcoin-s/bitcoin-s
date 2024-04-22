package org.bitcoins.rpc.common

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.testkit.rpc.BitcoindFixturesCachedPairNewest

class MessageRpcTest extends BitcoindFixturesCachedPairNewest {

  behavior of "MessageRpc"

  it should "be able to sign a message and verify that signature" in {
    nodePair =>
      val client = nodePair.node1
      val message = "Never gonna give you up\nNever gonna let you down\n..."
      for {
        address <- client.getNewAddress(addressType = AddressType.Legacy)
        signature <-
          client.signMessage(address.asInstanceOf[P2PKHAddress], message)
        validity <-
          client
            .verifyMessage(address.asInstanceOf[P2PKHAddress],
                           signature,
                           message)
      } yield assert(validity)
  }

  it should "be able to sign a message with a private key and verify that signature" in {
    nodePair =>
      val message = "Never gonna give you up\nNever gonna let you down\n..."
      val privKey = ECPrivateKey.freshPrivateKey
      val address = P2PKHAddress(privKey.publicKey, networkParam)
      val client = nodePair.node1
      for {
        signature <- client.signMessageWithPrivKey(
          privKey.toPrivateKeyBytes(),
          message
        )
        validity <- client.verifyMessage(address, signature, message)
      } yield assert(validity)
  }
}
