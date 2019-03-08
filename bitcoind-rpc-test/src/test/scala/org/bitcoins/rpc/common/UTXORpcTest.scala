package org.bitcoins.rpc.common

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.rpc.BitcoindRpcTestUtil
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, RpcOpts}
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

class UTXORpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem =
    ActorSystem("UTXORpcTest", BitcoindRpcTestUtil.AKKA_CONFIG)
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  lazy val accum: mutable.Builder[
    BitcoindRpcClient,
    Vector[BitcoindRpcClient]] = Vector.newBuilder[BitcoindRpcClient]

  lazy val clientF: Future[BitcoindRpcClient] =
    BitcoindRpcTestUtil.startedBitcoindRpcClient(clientAccum = accum)

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(accum.result)
    TestKit.shutdownActorSystem(system)
  }

  behavior of "UTXORpc"

  it should "be able to list utxos" in {
    for {
      client <- clientF
      unspent <- client.listUnspent
    } yield assert(unspent.nonEmpty)

  }

  it should "be able to lock and unlock utxos as well as list locked utxos" in {
    for {
      client <- clientF
      unspent <- client.listUnspent
      txid1 = unspent(0).txid
      txid2 = unspent(1).txid
      param = {

        val vout1 = unspent(0).vout
        val vout2 = unspent(1).vout
        Vector(RpcOpts.LockUnspentOutputParameter(txid1, vout1),
               RpcOpts.LockUnspentOutputParameter(txid2, vout2))
      }
      firstSuccess <- client.lockUnspent(unlock = false, param)
      locked <- client.listLockUnspent
      secondSuccess <- client.lockUnspent(unlock = true, param)
      newLocked <- client.listLockUnspent
    } yield {
      assert(firstSuccess)
      assert(locked.length == 2)
      assert(locked(0).txId.flip == txid1)
      assert(locked(1).txId.flip == txid2)
      assert(secondSuccess)
      assert(newLocked.isEmpty)
    }
  }

  it should "be able to get utxo info" in {
    for {
      client <- clientF
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
      info1 <- client.getTxOut(block.tx.head.txid, 0)
    } yield assert(info1.coinbase)
  }
}
