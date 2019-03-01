package org.bitcoins.rpc.v17

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import org.bitcoins.core.config.{NetworkParameters, RegTest}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.common.RpcOpts.{
  AddNodeArgument,
  AddressType,
  LabelPurpose
}
import org.bitcoins.rpc.client.v17.BitcoindV17RpcClient
import org.bitcoins.rpc.util.AsyncUtil
import org.bitcoins.rpc.{BitcoindRpcTestConfig, BitcoindRpcTestUtil}
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}
import org.slf4j.Logger

import scala.concurrent.{Await, ExecutionContext, Future}

class BitcoindV17RpcClientTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem(
    "BitcoindV17RpcClientTest_ActorSystem")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  val logger: Logger = BitcoinSLogger.logger

  val usedLabel = "used_label"
  val unusedLabel = "unused_label"

  val client = new BitcoindV17RpcClient(BitcoindRpcTestUtil.v17Instance())
  val otherClient = new BitcoindV17RpcClient(BitcoindRpcTestUtil.v17Instance())

  override protected def beforeAll(): Unit = {
    import BitcoindRpcTestConfig.DEFAULT_TIMEOUT

    val startF = BitcoindRpcTestUtil.startServers(Vector(client, otherClient))
    Await.result(startF, DEFAULT_TIMEOUT)

    val addNodeF =
      client.addNode(otherClient.getDaemon.uri, AddNodeArgument.Add)
    Await.result(addNodeF, DEFAULT_TIMEOUT)

    BitcoindRpcTestUtil.awaitConnection(client, otherClient)

    Await.result(client.generate(200), DEFAULT_TIMEOUT)
    Await.result(otherClient.generate(200), DEFAULT_TIMEOUT)
  }

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(Vector(client, otherClient))
    TestKit.shutdownActorSystem(system)
  }

  behavior of "BitcoindV17RpcClient"

  it should "sign a raw transaction with wallet keys" in {
    for {
      rawTx <- BitcoindRpcTestUtil.createRawCoinbaseTransaction(client,
                                                                otherClient)
      signedTx <- client.signRawTransactionWithWallet(rawTx)
    } yield assert(signedTx.complete)
  }

  it should "sign a raw transaction with private keys" ignore {
    ???
  }

  it should "be able to get the address info for a given address" in {
    client.getNewAddress.flatMap { addr =>
      client.getAddressInfo(addr).flatMap { _ =>
        succeed
      }
    }
  }

  it should "be able to get the address info for a given P2SHSegwit address" in {
    client.getNewAddress(addressType = AddressType.P2SHSegwit).flatMap { addr =>
      client.getAddressInfo(addr).flatMap { _ =>
        succeed
      }
    }
  }

  it should "be able to the address info for a given Legacy address" in {
    client.getNewAddress(addressType = AddressType.Legacy).flatMap { addr =>
      client.getAddressInfo(addr).flatMap { _ =>
        succeed
      }
    }
  }

  // needs #360 to be merged
  it should "be able to the address info for a given Bech32 address" ignore {
    for {
      addr <- client.getNewAddress(AddressType.Bech32)
      info <- client.getAddressInfo(addr)
    } yield assert(info.address.networkParameters == RegTest)
  }

  it should "be able to get the amount received by a label" in {
    client.getNewAddress(usedLabel).flatMap { address =>
      BitcoindRpcTestUtil
        .fundBlockChainTransaction(client, address, Bitcoins(1.5))
        .flatMap { _ =>
          {

            client.getReceivedByLabel(usedLabel).flatMap { amount =>
              assert(amount == Bitcoins(1.5))
            }
          }
        }
    }
  }

  it should "list all labels" in {
    client.listLabels().flatMap { _ =>
      succeed
    }
  }

  it should "list all labels with purposes" in {
    val sendLabel = "sendLabel"
    val numLabels = 2

    val isImportDone = () =>
      client.ping().map(_ => true).recover {
        case exc if exc.getMessage.contains("rescanning") => false
        case exc =>
          logger.error(s"throwing $exc")
          throw exc
    }

    def importTx(n: Int): Future[Unit] =
      for {
        address <- otherClient.getNewAddress
        _ <- client.importAddress(address, sendLabel + n)
        _ <- AsyncUtil.retryUntilSatisfiedF(isImportDone)
      } yield ()

    for {
      _ <- importTx(0)
      _ <- importTx(1)
      receiveLabels <- client.listLabels(Some(LabelPurpose.Receive))
      sendLabels <- client.listLabels(Some(LabelPurpose.Send))
    } yield assert(receiveLabels != sendLabels)

  }

  it should "set labels" in {
    val l = "setLabel"
    val btc = Bitcoins(1)
    for {
      addr <- client.getNewAddress
      _ <- BitcoindRpcTestUtil.fundBlockChainTransaction(otherClient, addr, btc)

      newestBlock <- otherClient.getBestBlockHash
      _ = AsyncUtil.awaitConditionF(() =>
        BitcoindRpcTestUtil.hasSeenBlock(client, newestBlock))

      oldAmount <- client.getReceivedByLabel(l)
      _ = assert(oldAmount == Bitcoins(0))
      _ <- client.setLabel(addr, l)
      newAmount <- client.getReceivedByLabel(l)

    } yield assert(newAmount == btc)
  }

  it should "list amounts received by all labels" in {
    for {
      addressWithLabel <- client.getNewAddress(usedLabel)
      addressNoLabel <- client.getNewAddress
      _ <- otherClient.sendToAddress(addressNoLabel, Bitcoins.one)
      _ <- otherClient.sendToAddress(addressWithLabel, Bitcoins.one)
      newBlock +: _ <- otherClient.generate(1)
      _ = AsyncUtil.awaitConditionF(() =>
        BitcoindRpcTestUtil.hasSeenBlock(client, newBlock))
      list <- client.listReceivedByLabel()
    } yield {

      val receivedToUsedlabel = list.find(_.label == usedLabel)

      assert(receivedToUsedlabel.isDefined)
      assert(receivedToUsedlabel.get.amount > Bitcoins.zero)

      val receivedDefaultLabel =
        list
          .find(_.label == "")

      assert(receivedDefaultLabel.isDefined)
      assert(receivedDefaultLabel.get.amount > Bitcoins.zero)

      assert(list.forall(_.label != unusedLabel))
    }
  }
}
