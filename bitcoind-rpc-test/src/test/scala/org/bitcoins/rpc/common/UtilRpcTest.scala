package org.bitcoins.rpc.common

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.rpc.BitcoindFixturesCachedPairNewest

import scala.concurrent.Future

class UtilRpcTest extends BitcoindFixturesCachedPairNewest {

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

  it should "get index info" in { nodePair =>
    val client = nodePair.node1
    def indexSynced(client: BitcoindRpcClient): Future[Boolean] = {
      client.getIndexInfo.map { indexes =>
        indexes("txindex").best_block_height == 101 && indexes(
          "basic block filter index"
        ).best_block_height == 101
      }
    }
    for {
      _ <- AsyncUtil.retryUntilSatisfiedF(() => indexSynced(client))
      indexes <- client.getIndexInfo
    } yield {
      val txIndexInfo = indexes("txindex")
      assert(txIndexInfo.synced)
      assert(txIndexInfo.best_block_height == 101)

      val blockFilterIndexInfo = indexes("basic block filter index")
      assert(blockFilterIndexInfo.synced)
      assert(blockFilterIndexInfo.best_block_height == 101)
    }
  }
}
