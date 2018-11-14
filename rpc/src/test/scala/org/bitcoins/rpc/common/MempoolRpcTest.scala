package org.bitcoins.rpc.common

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction.{ TransactionInput, TransactionOutPoint }
import org.bitcoins.rpc.TestUtil
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.scalatest.{ AsyncFlatSpec, BeforeAndAfterAll }

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, ExecutionContext }

class MempoolRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("MempoolRpcTest")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = TestUtil.network

  val client: BitcoindRpcClient = new BitcoindRpcClient(TestUtil.instance())
  val otherClient: BitcoindRpcClient = new BitcoindRpcClient(TestUtil.instance())

  override def beforeAll(): Unit = {
    TestUtil.startServers(client, otherClient)
    Await.result(client.generate(200), 3.seconds)
  }

  override protected def afterAll(): Unit = {
    TestUtil.stopServers(client, otherClient)
    Await.result(system.terminate(), 10.seconds)
  }

  behavior of "MempoolRpc"

  it should "be able to find a transaction sent to the mem pool" in {
    TestUtil.sendCoinbaseTransaction(client, otherClient).flatMap { transaction =>
      client.getRawMemPool.map { memPool =>
        assert(memPool.length == 1)
        assert(memPool.head == transaction.txid)
      }
    }
  }

  it should "be able to find a verbose transaction in the mem pool" in {
    TestUtil.sendCoinbaseTransaction(client, otherClient).flatMap { transaction =>
      client.getRawMemPoolWithTransactions.flatMap { memPool =>
        val txid = memPool.keySet.head
        assert(txid == transaction.txid)
        assert(memPool(txid).size > 0)
      }
    }
  }

  it should "be able to find a mem pool entry" in {
    TestUtil.sendCoinbaseTransaction(client, otherClient).flatMap { transaction =>
      client.getMemPoolEntry(transaction.txid).map { _ =>
        succeed
      }
    }
  }

  it should "be able to get mem pool info" in {
    client.generate(1).flatMap { _ =>
      client.getMemPoolInfo.flatMap { info =>
        assert(info.size == 0)
        TestUtil.sendCoinbaseTransaction(client, otherClient).flatMap { _ =>
          client.getMemPoolInfo.map { newInfo =>
            assert(newInfo.size == 1)
          }
        }
      }
    }
  }

  it should "be able to prioritise a mem pool transaction" in {
    otherClient.getNewAddress.flatMap { address =>
      TestUtil.fundMemPoolTransaction(client, address, Bitcoins(3.2)).flatMap { txid =>
        client.getMemPoolEntry(txid).flatMap { entry =>
          assert(entry.fee == entry.modifiedfee)
          client.prioritiseTransaction(txid, Bitcoins(1).satoshis).flatMap {
            tt =>
              assert(tt)
              client.getMemPoolEntry(txid).map { newEntry =>
                assert(newEntry.fee == entry.fee)
                assert(newEntry.modifiedfee == newEntry.fee + Bitcoins(1))
              }
          }
        }
      }
    }
  }

  it should "be able to find mem pool ancestors and descendants" in {
    client.generate(1)
    client.getNewAddress.flatMap { address =>
      TestUtil.fundMemPoolTransaction(client, address, Bitcoins(2)).flatMap { txid1 =>
        client.getRawMemPool.flatMap { mempool =>
          assert(mempool.head == txid1)
          client.getNewAddress.flatMap { address =>
            val input: TransactionInput =
              TransactionInput(
                TransactionOutPoint(txid1.flip, UInt32(0)),
                ScriptSignature.empty,
                UInt32.max - UInt32.one)
            client
              .createRawTransaction(Vector(input), Map(address -> Bitcoins(1)))
              .flatMap { createdTx =>
                client.signRawTransaction(createdTx).flatMap { signedTx =>
                  assert(signedTx.complete)
                  client.sendRawTransaction(signedTx.hex, allowHighFees = true).flatMap {
                    txid2 =>
                      client.getMemPoolDescendants(txid1).flatMap {
                        descendants =>
                          assert(descendants.head == txid2)
                          client
                            .getMemPoolDescendantsVerbose(txid1)
                            .flatMap { descendants =>
                              assert(descendants.head._1 == txid2)
                              assert(descendants.head._2.ancestorcount == 2)
                              client.getMemPoolAncestors(txid2).flatMap {
                                ancestors =>
                                  assert(ancestors.head == txid1)
                                  client
                                    .getMemPoolAncestorsVerbose(txid2)
                                    .map { ancestors =>
                                      assert(ancestors.head._1 == txid1)
                                      assert(
                                        ancestors.head._2.descendantcount == 2)
                                    }
                              }
                            }
                      }
                  }
                }
              }
          }
        }
      }
    }
  }
}

