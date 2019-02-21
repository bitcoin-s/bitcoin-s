package org.bitcoins.rpc.common

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  P2SHScriptSignature,
  ScriptPubKey,
  ScriptSignature
}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionInput,
  TransactionOutPoint
}
import org.bitcoins.rpc.{BitcoindRpcTestConfig, BitcoindRpcTestUtil}
import org.bitcoins.rpc.client.common.RpcOpts.AddNodeArgument
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, RpcOpts}
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.async.Async.{async, await}
import scala.concurrent.{Await, ExecutionContext}

class RawTransactionRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("RawTransactionRpcTest")
  implicit val ec: ExecutionContext = system.getDispatcher
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  implicit val client: BitcoindRpcClient = new BitcoindRpcClient(
    BitcoindRpcTestUtil.instance())
  val otherClient = new BitcoindRpcClient(BitcoindRpcTestUtil.instance())

  override def beforeAll(): Unit = {
    import BitcoindRpcTestConfig.DEFAULT_TIMEOUT

    val startF =
      BitcoindRpcTestUtil.startServers(Vector(client, otherClient))

    Await.result(startF, DEFAULT_TIMEOUT)

    val addNodeF =
      client.addNode(otherClient.getDaemon.uri, AddNodeArgument.Add)
    Await.result(addNodeF, DEFAULT_TIMEOUT)

    Await.result(client.generate(200), DEFAULT_TIMEOUT)
  }

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(Vector(client, otherClient))
    TestKit.shutdownActorSystem(system)
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
    BitcoindRpcTestUtil
      .createRawCoinbaseTransaction(client, otherClient)
      .flatMap { transaction =>
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

          assert(transaction1.blockhash.isDefined)
          assert(transaction2.blockhash.isDefined)

          for {
            hash1 <- transaction1.blockhash
            hash2 <- transaction2.blockhash
          } yield assert(hash1 == hash2)

          succeed
        }
      }
    }
  }

  it should "be able to create a raw transaction" in async {
    val txsF = for {
      blocks <- client.generate(2)
      firstBlock <- client.getBlock(blocks(0))
      firstTransaction <- client.getTransaction(firstBlock.tx(0))
      secondBlock <- client.getBlock(blocks(1))
      secondTransaction <- client.getTransaction(secondBlock.tx(0))
    } yield (firstTransaction, secondTransaction)

    val (transaction0, transaction1) = await(txsF)

    val input0 =
      TransactionOutPoint(transaction0.txid.flip,
                          UInt32(transaction0.blockindex.get))
    val input1 =
      TransactionOutPoint(transaction1.txid.flip,
                          UInt32(transaction1.blockindex.get))
    val sig: ScriptSignature = ScriptSignature.empty

    val address = await(otherClient.getNewAddress)

    val transaction = await({
      val inputs = Vector(TransactionInput(input0, sig, UInt32(1)),
                          TransactionInput(input1, sig, UInt32(2)))
      val outputs = Map(address -> Bitcoins(1))
      client.createRawTransaction(inputs, outputs)
    })

    val inputs = transaction.inputs
    assert(inputs.head.sequence == UInt32(1))
    assert(inputs(1).sequence == UInt32(2))
    assert(inputs.head.previousOutput.txId == input0.txId)
    assert(inputs(1).previousOutput.txId == input1.txId)
  }

  it should "be able to send a raw transaction to the mem pool" in {
    BitcoindRpcTestUtil
      .createRawCoinbaseTransaction(client, otherClient)
      .flatMap(BitcoindRpcTestUtil.signRawTransaction(client, _))
      .flatMap { signedTransaction =>
        client
          .generate(100)
          .flatMap { _ => // Can't spend coinbase until depth 100
            client
              .sendRawTransaction(signedTransaction.hex, allowHighFees = true)
              .map { _ =>
                succeed
              }
          }
      }
  }

  it should "be able to sign a raw transaction" in {
    client.getNewAddress.flatMap { address =>
      client.validateAddress(address).flatMap { addressInfo =>
        client
          .addMultiSigAddress(1, Vector(Left(addressInfo.pubkey.get)))
          .flatMap { multisig =>
            BitcoindRpcTestUtil
              .fundBlockChainTransaction(client,
                                         multisig.address,
                                         Bitcoins(1.2))
              .flatMap { txid =>
                client.getTransaction(txid).flatMap { rawTx =>
                  client.decodeRawTransaction(rawTx.hex).flatMap { tx =>
                    val output =
                      tx.vout
                        .find(output => output.value == Bitcoins(1.2))
                        .get
                    val input = TransactionInput(
                      TransactionOutPoint(txid.flip, UInt32(output.n)),
                      P2SHScriptSignature(multisig.redeemScript.hex),
                      UInt32.max - UInt32.one)
                    client.getNewAddress.flatMap { newAddress =>
                      client
                        .createRawTransaction(Vector(input),
                                              Map(newAddress -> Bitcoins(1.1)))
                        .flatMap {
                          BitcoindRpcTestUtil.signRawTransaction(
                            client,
                            _: Transaction,
                            Vector(RpcOpts.SignRawTransactionOutputParameter(
                              txid,
                              output.n,
                              ScriptPubKey.fromAsmHex(output.scriptPubKey.hex),
                              Some(multisig.redeemScript),
                              Bitcoins(1.2)))
                          )
                        }
                        .map { result =>
                          assert(result.complete)
                        }
                    }
                  }
                }
              }
          }
      }
    }
  }

  it should "be able to combine raw transactions" in async {
    val address1 = await(client.getNewAddress)
    val address2 = await(otherClient.getNewAddress)

    val address1Info = await(client.validateAddress(address1))
    val address2Info = await(otherClient.validateAddress(address2))

    val keys =
      Vector(Left(address1Info.pubkey.get), Left(address2Info.pubkey.get))

    val multisig = await(
      client
        .addMultiSigAddress(2, keys))

    await(
      otherClient
        .addMultiSigAddress(2, keys))

    await(client.validateAddress(multisig.address))

    val txid = await(
      BitcoindRpcTestUtil
        .fundBlockChainTransaction(client, multisig.address, Bitcoins(1.2)))

    val rawTx = await(client.getTransaction(txid))

    val tx = await(client.decodeRawTransaction(rawTx.hex))
    val output = tx.vout
      .find(output => output.value == Bitcoins(1.2))
      .get
    val input =
      TransactionInput(TransactionOutPoint(txid.flip, UInt32(output.n)),
                       P2SHScriptSignature(multisig.redeemScript.hex),
                       UInt32.max - UInt32.one)

    val address3 = await(client.getNewAddress)

    val ctx = await(
      otherClient
        .createRawTransaction(Vector(input), Map(address3 -> Bitcoins(1.1))))

    val txOpts =
      Vector(
        RpcOpts
          .SignRawTransactionOutputParameter(
            txid,
            output.n,
            ScriptPubKey.fromAsmHex(output.scriptPubKey.hex),
            Some(multisig.redeemScript),
            Bitcoins(1.2)))

    val partialTx1 =
      await(BitcoindRpcTestUtil.signRawTransaction(client, ctx, txOpts))

    assert(!partialTx1.complete)
    assert(partialTx1.hex != ctx)

    val partialTx2 =
      await(BitcoindRpcTestUtil.signRawTransaction(otherClient, ctx, txOpts))

    assert(!partialTx2.complete)
    assert(partialTx2.hex != ctx)

    val combinedTx = await(
      client
        .combineRawTransaction(Vector(partialTx1.hex, partialTx2.hex)))

    await(
      client
        .sendRawTransaction(combinedTx))

    succeed
  }

  it should "fail to abandon a transaction which has not been sent" in {
    otherClient.getNewAddress.flatMap { address =>
      client
        .createRawTransaction(Vector(), Map(address -> Bitcoins(1)))
        .flatMap { tx =>
          recoverToSucceededIf[RuntimeException](
            client.abandonTransaction(tx.txIdBE))
        }
    }
  }

  it should "be able to get a raw transaction in serialized form from the mem pool" in {
    BitcoindRpcTestUtil.sendCoinbaseTransaction(client, otherClient).flatMap {
      tx =>
        client.getRawTransactionRaw(tx.txid).map { transaction =>
          assert(transaction.txIdBE == tx.txid)
        }
    }
  }
}
