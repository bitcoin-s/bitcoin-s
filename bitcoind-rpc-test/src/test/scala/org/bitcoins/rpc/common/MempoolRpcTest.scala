package org.bitcoins.rpc.common

import java.io.File
import java.nio.file.Files

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import com.typesafe.config.ConfigValueFactory
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction.{
  TransactionInput,
  TransactionOutPoint
}
import org.bitcoins.rpc.{BitcoindRpcTestConfig, BitcoindRpcTestUtil}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.AddNodeArgument
import org.bitcoins.rpc.config.BitcoindInstance
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.async.Async.{async, await}
import scala.concurrent.{Await, ExecutionContext}

class MempoolRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("MempoolRpcTest")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  val client: BitcoindRpcClient = new BitcoindRpcClient(
    BitcoindRpcTestUtil.instance())

  val otherClient: BitcoindRpcClient = new BitcoindRpcClient(
    BitcoindRpcTestUtil.instance())

  override def beforeAll(): Unit = {
    import BitcoindRpcTestConfig.DEFAULT_TIMEOUT

    val startF = BitcoindRpcTestUtil.startServers(Vector(client, otherClient))
    Await.result(startF, DEFAULT_TIMEOUT)

    val addNodeF =
      client.addNode(otherClient.getDaemon.uri, AddNodeArgument.Add)
    Await.result(addNodeF, DEFAULT_TIMEOUT)

    BitcoindRpcTestUtil.awaitConnection(client, otherClient)
    Await.result(client.generate(200), DEFAULT_TIMEOUT)
  }

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(Vector(client, otherClient))
    TestKit.shutdownActorSystem(system)
  }

  behavior of "MempoolRpc"

  it should "be able to find a transaction sent to the mem pool" in {
    BitcoindRpcTestUtil.sendCoinbaseTransaction(client, otherClient).flatMap {
      transaction =>
        client.getRawMemPool.map { memPool =>
          assert(memPool.length == 1)
          assert(memPool.head == transaction.txid)
        }
    }
  }

  it should "be able to find a verbose transaction in the mem pool" in {
    BitcoindRpcTestUtil.sendCoinbaseTransaction(client, otherClient).flatMap {
      transaction =>
        client.getRawMemPoolWithTransactions.flatMap { memPool =>
          val txid = memPool.keySet.head
          assert(txid == transaction.txid)
          assert(memPool(txid).size > 0)
        }
    }
  }

  it should "be able to find a mem pool entry" in {
    BitcoindRpcTestUtil.sendCoinbaseTransaction(client, otherClient).flatMap {
      transaction =>
        client.getMemPoolEntry(transaction.txid).map { _ =>
          succeed
        }
    }
  }

  it should "be able to get mem pool info" in {
    client.generate(1).flatMap { _ =>
      client.getMemPoolInfo.flatMap { info =>
        assert(info.size == 0)
        BitcoindRpcTestUtil
          .sendCoinbaseTransaction(client, otherClient)
          .flatMap { _ =>
            client.getMemPoolInfo.map { newInfo =>
              assert(newInfo.size == 1)
            }
          }
      }
    }
  }

  it should "be able to prioritise a mem pool transaction" in async {
    val address = await(otherClient.getNewAddress)
    val txid =
      await(
        BitcoindRpcTestUtil
          .fundMemPoolTransaction(client, address, Bitcoins(3.2)))

    val entry = await(client.getMemPoolEntry(txid))
    assert(entry.fee == entry.modifiedfee)

    val tt = await(client.prioritiseTransaction(txid, Bitcoins(1).satoshis))
    assert(tt)

    val newEntry = await(client.getMemPoolEntry(txid))
    assert(newEntry.fee == entry.fee)
    assert(newEntry.modifiedfee == newEntry.fee + Bitcoins(1))
  }

  it should "be able to find mem pool ancestors and descendants" in async {
    client.generate(1)
    val address1 = await(client.getNewAddress)
    val txid1 =
      await(
        BitcoindRpcTestUtil
          .fundMemPoolTransaction(client, address1, Bitcoins(2)))
    val mempool = await(client.getRawMemPool)

    assert(mempool.head == txid1)
    val address2 = await(client.getNewAddress)

    val input: TransactionInput =
      TransactionInput(TransactionOutPoint(txid1.flip, UInt32(0)),
                       ScriptSignature.empty,
                       UInt32.max - UInt32.one)

    val createdTx = await(
      client
        .createRawTransaction(Vector(input), Map(address2 -> Bitcoins(1))))

    val signedTx =
      await(BitcoindRpcTestUtil.signRawTransaction(client, createdTx))
    assert(signedTx.complete)

    val txid2 = await(
      client
        .sendRawTransaction(signedTx.hex, allowHighFees = true))

    val descendantsTxid1 = await(client.getMemPoolDescendants(txid1))
    assert(descendantsTxid1.head == txid2)

    val verboseDescendantsTxid1 = await(
      client
        .getMemPoolDescendantsVerbose(txid1))
    assert(verboseDescendantsTxid1.head._1 == txid2)
    assert(verboseDescendantsTxid1.head._2.ancestorcount == 2)

    val ancestorsTxid2 = await(client.getMemPoolAncestors(txid2))
    assert(ancestorsTxid2.head == txid1)

    val verboseAncestorsTxid2 = await(
      client
        .getMemPoolAncestorsVerbose(txid2))
    assert(verboseAncestorsTxid2.head._1 == txid1)
    assert(verboseAncestorsTxid2.head._2.descendantcount == 2)
  }

  it should "be able to abandon a transaction" in {
    val defaultConfig = BitcoindRpcTestUtil.standardConfig

    val noBroadcastValue = ConfigValueFactory.fromAnyRef(0)
    val datadirValue = {
      val tempDirPrefix = null // because java APIs are bad
      val tempdirPath = Files.createTempDirectory(tempDirPrefix).toString
      ConfigValueFactory.fromAnyRef(tempdirPath)
    }

    val configNoBroadcast =
      defaultConfig
        .withValue("walletbroadcast", noBroadcastValue)
        .withValue("datadir", datadirValue)

    val _ = BitcoindRpcTestUtil.writeConfigToFile(configNoBroadcast)
    val instanceWithoutBroadcast =
      BitcoindInstance.fromConfig(configNoBroadcast)

    // walletbroadcast must be turned off for a transaction to be abondonable
    val clientWithOutBroadcast = new BitcoindRpcClient(instanceWithoutBroadcast)

    for {
      _ <- clientWithOutBroadcast.start()
      _ <- clientWithOutBroadcast.addNode(otherClient.getDaemon.uri,
                                          AddNodeArgument.Add)
      _ = BitcoindRpcTestUtil.awaitConnection(clientWithOutBroadcast,
                                              otherClient)
      _ <- clientWithOutBroadcast.generate(150) // to ensure we have enough balance
      recipient <- otherClient.getNewAddress
      txid <- clientWithOutBroadcast.sendToAddress(recipient, Bitcoins(1))
      _ <- clientWithOutBroadcast.abandonTransaction(txid)
      maybeAbandoned <- clientWithOutBroadcast.getTransaction(txid)
      _ <- clientWithOutBroadcast.stop()
    } yield assert(maybeAbandoned.details.head.abandoned.contains(true))
  }

  it should "be able to save the mem pool to disk" in {
    val regTest =
      new File(client.getDaemon.authCredentials.datadir + "/regtest")
    assert(regTest.isDirectory)
    assert(!regTest.list().contains("mempool.dat"))
    client.saveMemPool().map { _ =>
      assert(regTest.list().contains("mempool.dat"))
    }
  }
}
