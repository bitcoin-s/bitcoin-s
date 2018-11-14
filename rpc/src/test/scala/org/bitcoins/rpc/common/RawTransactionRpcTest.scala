package org.bitcoins.rpc.common

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction.{ TransactionInput, TransactionOutPoint }
import org.bitcoins.rpc.BitcoindRpcTestUtil
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.AddNodeArgument
import org.scalatest.{ AsyncFlatSpec, BeforeAndAfterAll }

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, ExecutionContext }

class RawTransactionRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("RawTransactionRpcTest")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  implicit val client: BitcoindRpcClient = new BitcoindRpcClient(BitcoindRpcTestUtil.instance())
  val otherClient = new BitcoindRpcClient(BitcoindRpcTestUtil.instance())
  val clients = Vector(client, otherClient)

  override def beforeAll(): Unit = {
    BitcoindRpcTestUtil.startServers(clients)
    Await.result(client.addNode(otherClient.getDaemon.uri, AddNodeArgument.Add), 3.seconds)
    Await.result(client.generate(200), 3.seconds)
  }

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(clients)
    Await.result(system.terminate(), 10.seconds)
  }

  behavior of "RawTransactionRpc"

  it should "be able to fund a raw transaction" in {
    otherClient.getNewAddress.flatMap { address =>
      client
        .createRawTransaction(Vector.empty, Map(address -> Bitcoins(1)))
        .flatMap { transactionWithoutFunds =>
          client.fundRawTransaction(transactionWithoutFunds).flatMap {
            transactionResult =>
              val transaction = transactionResult.hex
              assert(transaction.inputs.length == 1)
              client
                .getRawTransaction(
                  transaction.inputs.head.previousOutput.txId.flip)
                .flatMap { inputTransaction =>
                  assert(
                    inputTransaction
                      .vout(transaction.inputs.head.previousOutput.vout.toInt)
                      .value
                      .satoshis
                      .toBigInt ==
                      transactionResult.fee.satoshis.toBigInt +
                      transaction.outputs.head.value.satoshis.toBigInt +
                      transaction.outputs(1).value.satoshis.toBigInt)
                }
          }
        }
    }
  }

  it should "be able to decode a raw transaction" in {
    BitcoindRpcTestUtil.createRawCoinbaseTransaction(client, otherClient).flatMap { transaction =>
      client.decodeRawTransaction(transaction).map { rpcTransaction =>
        assert(rpcTransaction.txid == transaction.txIdBE)
        assert(rpcTransaction.locktime == transaction.lockTime)
        assert(rpcTransaction.size == transaction.size)
        assert(rpcTransaction.version == transaction.version.toInt)
        assert(rpcTransaction.vsize == transaction.vsize)
      }
    }
  }

  it should "be able to get a raw transaction using both rpcs available" in {
    BitcoindRpcTestUtil.getFirstBlock.flatMap { block =>
      val txid = block.tx.head.txid
      client.getRawTransaction(txid).flatMap { transaction1 =>
        client.getTransaction(txid).map { transaction2 =>
          assert(transaction1.txid == transaction2.txid)
          assert(transaction1.confirmations == transaction2.confirmations)
          assert(transaction1.hex == transaction2.hex)
          assert(transaction2.blockhash.contains(transaction1.blockhash))
          assert(transaction2.blocktime.contains(transaction1.blocktime))
        }
      }
    }
  }

  it should "be able to create a raw transaction" in {
    client.generate(2).flatMap { blocks =>
      client.getBlock(blocks(0)).flatMap { block0 =>
        client.getBlock(blocks(1)).flatMap { block1 =>
          client.getTransaction(block0.tx(0)).flatMap { transaction0 =>
            client.getTransaction(block1.tx(0)).flatMap { transaction1 =>
              val input0 =
                TransactionOutPoint(
                  transaction0.txid.flip,
                  UInt32(transaction0.blockindex.get))
              val input1 =
                TransactionOutPoint(
                  transaction1.txid.flip,
                  UInt32(transaction1.blockindex.get))
              val sig: ScriptSignature = ScriptSignature.empty
              otherClient.getNewAddress.flatMap { address =>
                client
                  .createRawTransaction(
                    Vector(
                      TransactionInput(input0, sig, UInt32(1)),
                      TransactionInput(input1, sig, UInt32(2))),
                    Map((address, Bitcoins(1))))
                  .map { transaction =>
                    assert(transaction.inputs.head.sequence == UInt32(1))
                    assert(transaction.inputs(1).sequence == UInt32(2))
                    assert(transaction
                      .inputs.head
                      .previousOutput
                      .txId == input0.txId)
                    assert(transaction
                      .inputs(1)
                      .previousOutput
                      .txId == input1.txId)
                  }
              }
            }
          }
        }
      }
    }
  }

  it should "be able to sign a raw transaction with wallet keys" in {
    BitcoindRpcTestUtil.createRawCoinbaseTransaction(client, otherClient).flatMap { transaction =>
      client.signRawTransaction(transaction).map { signedTransaction =>
        assert(signedTransaction.complete)
      }
    }
  }

  it should "be able to send a raw transaction to the mem pool" in {
    BitcoindRpcTestUtil.createRawCoinbaseTransaction(client, otherClient).flatMap { transaction =>
      client.signRawTransaction(transaction).flatMap { signedTransaction =>
        client
          .generate(100)
          .flatMap { _ => // Can't spend coinbase until depth 100
            client.sendRawTransaction(signedTransaction.hex, allowHighFees = true).map {
              _ =>
                succeed
            }
          }
      }
    }
  }

  it should "be able to get a raw transaction in serialized form from the mem pool" in {
    BitcoindRpcTestUtil.sendCoinbaseTransaction(client, otherClient).flatMap { tx =>
      client.getRawTransactionRaw(tx.txid).map { transaction =>
        assert(transaction.txIdBE == tx.txid)
      }
    }
  }
}
