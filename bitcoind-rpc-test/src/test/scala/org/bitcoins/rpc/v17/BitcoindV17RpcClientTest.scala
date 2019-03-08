package org.bitcoins.rpc.v17

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.bitcoins.core.config.{NetworkParameters, RegTest}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.BitcoindRpcTestUtil
import org.bitcoins.rpc.client.common.RpcOpts.{AddressType, LabelPurpose}
import org.bitcoins.rpc.client.v17.BitcoindV17RpcClient
import org.bitcoins.rpc.util.AsyncUtil
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}
import org.slf4j.Logger

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

class BitcoindV17RpcClientTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem(
    "BitcoindV17RpcClientTest_ActorSystem",
    BitcoindRpcTestUtil.AKKA_CONFIG)
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  val logger: Logger = BitcoinSLogger.logger

  val usedLabel = "used_label"
  val unusedLabel = "unused_label"

  val accum: mutable.Builder[
    BitcoindV17RpcClient,
    Vector[BitcoindV17RpcClient]] = Vector.newBuilder[BitcoindV17RpcClient]

  val clientsF: Future[(BitcoindV17RpcClient, BitcoindV17RpcClient)] =
    BitcoindRpcTestUtil.createNodePairV17(accum)

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(accum.result)
    TestKit.shutdownActorSystem(system)
  }

  behavior of "BitcoindV17RpcClient"

  it should "sign a raw transaction with wallet keys" in {
    for {
      (client, otherClient) <- clientsF
      rawTx <- BitcoindRpcTestUtil.createRawCoinbaseTransaction(client,
                                                                otherClient)
      signedTx <- client.signRawTransactionWithWallet(rawTx)
    } yield assert(signedTx.complete)
  }

  it should "sign a raw transaction with private keys" ignore {
    ???
  }

  it should "be able to get the address info for a given address" in {
    for {
      (client, _) <- clientsF
      addr <- client.getNewAddress
      _ <- client.getAddressInfo(addr)
    } yield succeed
  }

  it should "be able to get the address info for a given P2SHSegwit address" in {
    for {
      (client, _) <- clientsF
      addr <- client.getNewAddress(addressType = AddressType.P2SHSegwit)
      _ <- client.getAddressInfo(addr)
    } yield succeed
  }

  it should "be able to the address info for a given Legacy address" in {
    for {
      (client, _) <- clientsF
      addr <- client.getNewAddress(addressType = AddressType.Legacy)
      _ <- client.getAddressInfo(addr)
    } yield succeed
  }

  // needs #360 to be merged
  it should "be able to the address info for a given Bech32 address" ignore {
    for {
      (client, _) <- clientsF
      addr <- client.getNewAddress(AddressType.Bech32)
      info <- client.getAddressInfo(addr)
    } yield assert(info.address.networkParameters == RegTest)
  }

  it should "be able to get the amount received by a label" in {
    for {
      (client, _) <- clientsF
      address <- client.getNewAddress(usedLabel)
      _ <- BitcoindRpcTestUtil
        .fundBlockChainTransaction(client, address, Bitcoins(1.5))

      amount <- client.getReceivedByLabel(usedLabel)
    } yield assert(amount == Bitcoins(1.5))
  }

  it should "list all labels" in {
    for {
      (client, _) <- clientsF
      _ <- client.listLabels()
    } yield succeed
  }

  it should "list all labels with purposes" in {
    clientsF.flatMap {
      case (client, otherClient) =>
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
  }

  it should "set labels" in {
    val l = "setLabel"
    val btc = Bitcoins(1)
    for {
      (client, otherClient) <- clientsF
      addr <- client.getNewAddress
      _ <- BitcoindRpcTestUtil.fundBlockChainTransaction(otherClient, addr, btc)

      newestBlock <- otherClient.getBestBlockHash
      _ <- AsyncUtil.retryUntilSatisfiedF(() =>
        BitcoindRpcTestUtil.hasSeenBlock(client, newestBlock))

      oldAmount <- client.getReceivedByLabel(l)
      _ = assert(oldAmount == Bitcoins(0))
      _ <- client.setLabel(addr, l)
      newAmount <- client.getReceivedByLabel(l)

    } yield assert(newAmount == btc)
  }

  it should "list amounts received by all labels" in {
    for {
      (client, otherClient) <- clientsF
      addressWithLabel <- client.getNewAddress(usedLabel)
      addressNoLabel <- client.getNewAddress
      _ <- otherClient.sendToAddress(addressNoLabel, Bitcoins.one)
      _ <- otherClient.sendToAddress(addressWithLabel, Bitcoins.one)
      newBlock +: _ <- otherClient.generate(1)
      _ <- AsyncUtil.retryUntilSatisfiedF(() =>
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
