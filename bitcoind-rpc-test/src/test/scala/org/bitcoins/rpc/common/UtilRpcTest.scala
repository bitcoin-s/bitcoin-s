package org.bitcoins.rpc.common

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.core.protocol.{BitcoinAddress, P2PKHAddress}
import org.bitcoins.core.protocol.script.descriptor.Descriptor
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

  it should "return active rpc commands" in { nodePair =>
    val client = nodePair.node1
    val generatedF =
      client.getNewAddress.flatMap(addr => client.generateToAddress(100, addr))
    val rpcinfoF =
      generatedF.flatMap(_ => client.getRpcInfo())

    rpcinfoF.map { result =>
      assert(result.active_commands.length == 1)
    }
  }

  it should "derive addresses from a descriptor" in { case nodePair =>
    val client = nodePair.node1
    val str0 =
      "wpkh(tprv8ZgxMBicQKsPd7Uf69XL1XwhmjHopUGep8GuEiJDZmbQz6o58LninorQAfcKZWARbtRtfnLcJ5MQ2AtHcQJCCRUcMRvmDUjyEmNUWwx8UbK/1/1/0)#t6wfjs64"
    val descriptor0 = Descriptor.fromString(str0)
    assert(descriptor0.toString == str0)
    val addresses0F =
      client.deriveAddresses(descriptor0, None).map(_.addresses)
    val expected0 =
      Vector("bcrt1qjqmxmkpmxt80xz4y3746zgt0q3u3ferr34acd5").map(
        BitcoinAddress.fromString
      )
    val assert0 = addresses0F.map { addresses =>
      assert(addresses == expected0)
    }

    val str1 =
      "wpkh(tprv8ZgxMBicQKsPd7Uf69XL1XwhmjHopUGep8GuEiJDZmbQz6o58LninorQAfcKZWARbtRtfnLcJ5MQ2AtHcQJCCRUcMRvmDUjyEmNUWwx8UbK/1/1/*)#kft60nuy"

    val descriptor1 = Descriptor.fromString(str1)
    assert(descriptor1.toString == str1)
    val addresses1F =
      client.deriveAddresses(descriptor1, Some(Vector(0, 2))).map(_.addresses)
    val expected1 =
      Vector(
        "bcrt1qjqmxmkpmxt80xz4y3746zgt0q3u3ferr34acd5",
        "bcrt1qhku5rq7jz8ulufe2y6fkcpnlvpsta7rq4442dy",
        "bcrt1qpgptk2gvshyl0s9lqshsmx932l9ccsv265tvaq"
      )
        .map(BitcoinAddress.fromString)

    val assert1 = assert0.flatMap(_ =>
      addresses1F.map { addresses =>
        assert(addresses == expected1)
      })

    assert1
  }
}
