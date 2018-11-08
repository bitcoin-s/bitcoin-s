package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.BitcoindV17RpcClient
import org.scalatest.{ AsyncFlatSpec, BeforeAndAfterAll }
import org.slf4j.Logger

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, ExecutionContext }

class BitcoindV17RpcClientTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("BitcoindV16RpcClientTest_ActorSystem")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = TestUtil.network

  val logger: Logger = BitcoinSLogger.logger

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

  it should "be able to get the amount received by a label and list amounts received by all labels" in {
    val account = "another_new_account"
    val emptyAccount = "empty_account"
    client.getNewAddress(account).flatMap { address =>
      TestUtil.fundBlockChainTransaction(client, address, Bitcoins(1.5)).flatMap {
        _ =>
          client.getReceivedByLabel(account).flatMap { amount =>
            assert(amount == Bitcoins(1.5))
            client.listReceivedByAccount().flatMap { list =>
              logger.error(s"Got list $list")
              assert(
                list
                  .find(acc => acc.account == account)
                  .get
                  .amount == Bitcoins(1.5))
              assert(
                list
                  .find(acc => acc.account == "")
                  .get
                  .amount > Bitcoins(0))
              assert(!list.exists(acc => acc.account == emptyAccount))
              client.listAccounts().map { map =>
                assert(map(account) == Bitcoins(1.5))
                assert(map("") > Bitcoins(0))
                assert(!map.keySet.contains(emptyAccount))
              }
            }
          }
      }
    }
  }
}
