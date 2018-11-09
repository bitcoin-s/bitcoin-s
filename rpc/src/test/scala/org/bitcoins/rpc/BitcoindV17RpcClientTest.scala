package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.BitcoindV17RpcClient
import org.bitcoins.rpc.client.RpcOpts.AddressType
import org.scalatest.{ AsyncFlatSpec, BeforeAndAfterAll }
import org.slf4j.Logger

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, ExecutionContext }

class BitcoindV17RpcClientTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("BitcoindV17RpcClientTest_ActorSystem")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = TestUtil.network

  val logger: Logger = BitcoinSLogger.logger

  val label = "another_new_label"
  val otherLabel = "other_label"

  val client = new BitcoindV17RpcClient(TestUtil.instance())

  override protected def beforeAll(): Unit = {
    logger.info("Starting MessageRpcTest")
    logger.info("Bitcoin server starting")
    TestUtil.startServers(client)
    logger.info("Bitcoin server started")

    logger.info("Funding wallet by mining some blocks")
    Await.result(client.generate(200), 3.seconds)
  }

  override protected def afterAll(): Unit = {
    logger.info("Cleaning up after MessageRpcTest")

    logger.info("Stopping Bitcoin servers")
    TestUtil.stopServers(client)
    logger.info("Bitcoin servers stopped")

    logger.info("Stopping ActorSystem")
    Await.result(system.terminate(), 10.seconds)
    logger.info("Stopped ActorSystem")
  }

  behavior of "BitcoindV17RpcClient"

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

  // TODO There's an issue here with how Bech32Addr.value works,
  // it results in "UndefinedHRP(ByteVector(4 bytes, 0x62637274))1qm5zxhmrm4nnphye72rsnz9g5wna656mn5xyvw0"
  it should "be able to the address info for a given Bech32 address" in {
    client.getNewAddress(addressType = AddressType.Bech32).flatMap { addr =>
      client.getAddressInfo(addr).flatMap { _ =>
        succeed
      }
    }
  }

  it should "be able to get the amount received by a label" in {
    client.getNewAddress(label).flatMap { address =>
      TestUtil.fundBlockChainTransaction(client, address, Bitcoins(1.5)).flatMap {
        _ =>
          client.getReceivedByLabel(label).flatMap { amount =>
            assert(amount == Bitcoins(1.5))
            client.listReceivedByLabel().flatMap { list =>
              assert(
                list
                  .find(l => l.label == label)
                  .get
                  .amount == Bitcoins(1.5))
              assert(
                list
                  .find(l => l.label == "")
                  .get
                  .amount > Bitcoins(0))
              assert(!list.exists(acc => acc.account == otherLabel))
            }
          }
      }
    }
  }

  it should "list amounts received by all labels" in {
    client.listLabels().flatMap { labels =>
      assert(labels.contains(label))
      assert(!labels.contains(otherLabel))
    }
  }
}
