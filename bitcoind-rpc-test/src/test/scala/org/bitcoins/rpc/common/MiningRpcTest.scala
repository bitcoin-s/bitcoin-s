package org.bitcoins.rpc.common

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.rpc.BitcoindRpcTestUtil
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

class MiningRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem =
    ActorSystem("MiningRpcTest", BitcoindRpcTestUtil.AKKA_CONFIG)
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  val clientAccum: mutable.Builder[
    BitcoindRpcClient,
    Vector[BitcoindRpcClient]] = Vector.newBuilder[BitcoindRpcClient]

  lazy val clientsF: Future[(BitcoindRpcClient, BitcoindRpcClient)] =
    BitcoindRpcTestUtil.createNodePair(clientAccum = clientAccum)

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(clientAccum.result)
    TestKit.shutdownActorSystem(system)
  }

  behavior of "MiningRpc"

  it should "be able to get a block template" in {
    clientsF.flatMap {
      case (client, _) =>
        val getBlockF = client.getBlockTemplate()
        getBlockF
          .recover {
            // getblocktemplate is having a bad time on regtest
            // https://github.com/bitcoin/bitcoin/issues/11379
            case err: Throwable
                if err.getMessage
                  .contains("-9") =>
              succeed
            case other: Throwable => throw other
          }
          .map(_ => succeed)
    }
  }

  it should "be able to generate blocks" in {
    for {
      (client, _) <- clientsF
      blocks <- client.generate(3)
    } yield assert(blocks.length == 3)
  }

  it should "be able to get the mining info" in {
    for {
      (client, _) <- clientsF
      info <- client.getMiningInfo
    } yield assert(info.chain == "regtest")
  }

  it should "be able to generate blocks to an address" in {
    for {
      (client, otherClient) <- clientsF
      address <- otherClient.getNewAddress
      blocks <- client.generateToAddress(3, address)
      foundBlocks <- {
        val hashFuts = blocks.map(client.getBlockWithTransactions)
        Future.sequence(hashFuts)
      }
    } yield {
      assert(blocks.length == 3)
      assert(blocks.length == 3)
      foundBlocks.foreach { found =>
        assert(
          found.tx.head.vout.head.scriptPubKey.addresses.get.head == address)
      }
      succeed
    }
  }

  it should "be able to generate blocks and then get their serialized headers" in {
    for {
      (client, _) <- clientsF
      blocks <- client.generate(2)
      header <- client.getBlockHeaderRaw(blocks(1))
    } yield assert(header.previousBlockHashBE == blocks(0))
  }

  it should "be able to generate blocks and then get their headers" in {
    for {
      (client, _) <- clientsF
      blocks <- client.generate(2)
      firstHeader <- client.getBlockHeader(blocks(0))
      secondHeader <- client.getBlockHeader(blocks(1))
    } yield {
      assert(firstHeader.nextblockhash.contains(blocks(1)))
      assert(secondHeader.previousblockhash.contains(blocks(0)))
      assert(secondHeader.nextblockhash.isEmpty)
    }
  }

  it should "be able to get the network hash per sec" in {
    for {
      (client, _) <- clientsF
      hps <- client.getNetworkHashPS()
    } yield assert(hps > 0)
  }
}
