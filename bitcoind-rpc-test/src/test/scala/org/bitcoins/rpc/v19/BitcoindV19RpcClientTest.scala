package org.bitcoins.rpc.v19
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest

import scala.concurrent.Future

class BitcoindV19RpcClientTest extends BitcoindRpcTest {
  lazy val clientF: Future[BitcoindV19RpcClient] = {
    val client = new BitcoindV19RpcClient(BitcoindRpcTestUtil.v19Instance())
    val clientIsStartedF = BitcoindRpcTestUtil.startServers(Vector(client))
    clientIsStartedF.map(_ => client)
  }
  lazy val clientPairF: Future[(BitcoindV19RpcClient, BitcoindV19RpcClient)] =
    BitcoindRpcTestUtil.createNodePairV19(clientAccum)

  clientF.foreach(c => clientAccum.+=(c))

  behavior of "BitcoindV19RpcClient"

  it should "be able to start a V19 bitcoind instance" in {

    clientF.map { client =>
      assert(client.version == BitcoindVersion.V19)
    }

  }

  it should "get a block filter given a block hash" in {
    for {
      (client, _) <- clientPairF
      blocks <- client.getNewAddress.flatMap(client.generateToAddress(1, _))
      blockFilter <- client.getBlockFilter(blocks.head.hex, "basic")
    } yield {
      assert(blockFilter.filter.length > 0)
      assert(blockFilter.header.length > 0)
    }
  }

  it should "be able to get the balances" in {
    for {
      (client, _) <- clientPairF
      immatureBalance <- client.getBalances
      _ <- client.getNewAddress.flatMap(client.generateToAddress(1, _))
      newImmatureBalance <- client.getBalances
    } yield {
      assert(immatureBalance.mine.immature.toBigDecimal > 0)
      assert(immatureBalance.mine.immature < newImmatureBalance.mine.immature)
    }
  }

  it should "be able to set the wallet flag 'avoid_reuse'" in {
    for {
      (client, _) <- clientPairF
      result <- client.setWalletFlag("avoid_reuse", value = true)
    } yield {
      assert(result.flag == "avoid_reuse")
      assert(result.state)
    }
  }
}
