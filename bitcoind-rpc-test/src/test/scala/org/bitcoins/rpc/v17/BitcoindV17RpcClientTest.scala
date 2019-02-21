package org.bitcoins.rpc.v17

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.common.RpcOpts.{AddressType, LabelPurpose}
import org.bitcoins.rpc.client.v17.BitcoindV17RpcClient
import org.bitcoins.rpc.{BitcoindRpcTestConfig, BitcoindRpcTestUtil}
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}
import org.slf4j.Logger

import scala.async.Async.{async, await}
import scala.concurrent.{Await, ExecutionContext, Future}

class BitcoindV17RpcClientTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem(
    "BitcoindV17RpcClientTest_ActorSystem")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  val logger: Logger = BitcoinSLogger.logger

  val label = "another_new_label"
  val otherLabel = "other_label"

  lazy val client = new BitcoindV17RpcClient(BitcoindRpcTestUtil.instance())
  lazy val otherClient = new BitcoindV17RpcClient(
    BitcoindRpcTestUtil.instance())

  private val DEFAULT_TIMEOUT = BitcoindRpcTestConfig.DEFAULT_TIMEOUT

  override protected def beforeAll(): Unit = {
    // BitcoindRpcTestUtil.startServers(client, otherClient)

    // Await.result(client.generate(200), DEFAULT_TIMEOUT)
    // Await.result(otherClient.generate(200), DEFAULT_TIMEOUT)
  }

  override protected def afterAll(): Unit = {
    // BitcoindRpcTestUtil.stopServers(client, otherClient)
    TestKit.shutdownActorSystem(system)
  }

  behavior of "BitcoindV17RpcClient"

  it should "sign a raw transaction with wallet keys" ignore async {
    val rawTx = await(
      BitcoindRpcTestUtil.createRawCoinbaseTransaction(client, otherClient))
    val signedTx = await(client.signRawTransactionWithWallet(rawTx))
    assert(signedTx.complete)
  }

  it should "sign a raw transaction with private keys" ignore {
    ???
  }

  it should "be able to get the address info for a given address" ignore {
    client.getNewAddress.flatMap { addr =>
      client.getAddressInfo(addr).flatMap { _ =>
        succeed
      }
    }
  }

  it should "be able to get the address info for a given P2SHSegwit address" ignore {
    client.getNewAddress(addressType = AddressType.P2SHSegwit).flatMap { addr =>
      client.getAddressInfo(addr).flatMap { _ =>
        succeed
      }
    }
  }

  it should "be able to the address info for a given Legacy address" ignore {
    client.getNewAddress(addressType = AddressType.Legacy).flatMap { addr =>
      client.getAddressInfo(addr).flatMap { _ =>
        succeed
      }
    }
  }

  // TODO There's an issue here with how Bech32Addr.value works,
  // it results in "UndefinedHRP(ByteVector(4 bytes, 0x62637274))1qm5zxhmrm4nnphye72rsnz9g5wna656mn5xyvw0"
  it should "be able to the address info for a given Bech32 address" ignore {
    client.getNewAddress(addressType = AddressType.Bech32).flatMap { addr =>
      client.getAddressInfo(addr).flatMap { _ =>
        succeed
      }
    }
  }

  it should "be able to get the amount received by a label" ignore {
    client.getNewAddress(label).flatMap { address =>
      BitcoindRpcTestUtil
        .fundBlockChainTransaction(client, address, Bitcoins(1.5))
        .flatMap { _ =>
          {

            client.getReceivedByLabel(label).flatMap { amount =>
              logger.error(s"$label has 1.5 BTC")
              assert(amount == Bitcoins(1.5))
            }
          }
        }
    }
  }

  it should "list all labels" ignore {
    client.listLabels().flatMap { _ =>
      succeed
    }
  }

  it should "list all labels with purpose receive" ignore async {
    val sendLabel = "sendLabel"
    val numLabels = 2
    val importTx = (n: Int) =>
      async {
        val addr = await(otherClient.getNewAddress)
        client.importAddress(addr, sendLabel + n)
    }

    val futures = (0 to numLabels).toList.map(importTx)

    await(Future.sequence(futures))

    val labels = await(client.listLabels(Some(LabelPurpose.Receive)))
    assert(labels.length == numLabels)
  }

  it should "set labels" ignore async {
    val l = "setLabel"
    val btc = Bitcoins(1)
    val addr = await(client.getNewAddress)
    val _ = await(
      BitcoindRpcTestUtil.fundBlockChainTransaction(otherClient, addr, btc))
    val oldAmount = await(client.getReceivedByLabel(l))

    assert(oldAmount == Bitcoins(0))

    await(client.setLabel(addr, l))

    val newAmount = await(client.getReceivedByLabel(l))

    assert(newAmount == btc)
  }

  it should "list all labels with purpose \"send\"" ignore {
    client.listLabels(Some(LabelPurpose.Send)).flatMap { _ =>
      succeed
    }
  }

  it should "list amounts received by all labels" ignore {
    client.listReceivedByLabel().flatMap { list =>
      assert(
        list
          .find(l => l.label == label)
          .get
          .amount == Bitcoins(1.5))
      logger.error(s"$label is in list of labels")
      assert(
        list
          .find(l => l.label == "")
          .get
          .amount > Bitcoins(0))
      logger.error(s"the empty label has 0 BTC")
      assert(!list.exists(acc => acc.account == otherLabel))
      logger.error(s"$otherLabel does not exist")
      succeed
    }
  }
}
