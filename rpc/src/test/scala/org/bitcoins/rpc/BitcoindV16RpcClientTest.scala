package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.BitcoindV16RpcClient
import org.scalatest.{ AsyncFlatSpec, BeforeAndAfterAll }
import org.slf4j.Logger

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, ExecutionContext }

class BitcoindV16RpcClientTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("BitcoindV16RpcClientTest_ActorSystem")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = TestUtil.network

  val logger: Logger = BitcoinSLogger.logger

  val client = new BitcoindV16RpcClient(TestUtil.instance())
  val otherClient = new BitcoindV16RpcClient(TestUtil.instance())

  override protected def beforeAll(): Unit = {
    logger.info("Starting MessageRpcTest")
    logger.info("Bitcoin server starting")
    TestUtil.startServers(client, otherClient)
    logger.info("Bitcoin server started")

    client.addNode(otherClient.getDaemon.uri, "add")

    logger.info("Funding wallet by mining some blocks")
    Await.result(client.generate(200), 3.seconds)
  }

  override protected def afterAll(): Unit = {
    logger.info("Cleaning up after MessageRpcTest")

    logger.info("Stopping Bitcoin servers")
    TestUtil.stopServers(client, otherClient)
    logger.info("Bitcoin servers stopped")

    logger.info("Stopping ActorSystem")
    Await.result(system.terminate(), 10.seconds)
    logger.info("Stopped ActorSystem")

  }

  behavior of "BitoindV16RpcClient"

  it should "be able to send from an account to an addresss" in {
    otherClient.getNewAddress.flatMap { address =>
      client.sendFrom("", address, Bitcoins(1)).flatMap { txid =>
        client.getTransaction(txid).map { transaction =>
          assert(transaction.amount == Bitcoins(-1))
          assert(transaction.details.head.address.contains(address))
        }
      }
    }
  }

  it should "be able to get the amount received by an account and list amounts received by all accounts" in {
    val account = "another_new_account"
    val emptyAccount = "empty_account"
    client.getNewAddress(account).flatMap { address =>
      TestUtil.fundBlockChainTransaction(client, address, Bitcoins(1.5)).flatMap {
        _ =>
          client.getReceivedByAccount(account).flatMap { amount =>
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

  it should "be able to get and set the account for a given address" in {
    val account1 = "account_1"
    val account2 = "account_2"
    client.getNewAddress(account1).flatMap { address =>
      client.getAccount(address).flatMap { acc1 =>
        assert(acc1 == account1)
        client.setAccount(address, account2).flatMap { _ =>
          client.getAccount(address).map { acc2 =>
            assert(acc2 == account2)
          }
        }
      }
    }
  }

  it should "be able to get all addresses belonging to an account" in {
    client.getNewAddress.flatMap { address =>
      client.getAddressesByAccount("").map { addresses =>
        assert(addresses.contains(address))
      }
    }
  }

  it should "be able to get an account's address" in {
    val account = "a_new_account"
    client.getAccountAddress(account).flatMap { address =>
      client.getAccount(address).map { result =>
        assert(result == account)
      }
    }
  }

  it should "be able to move funds from one account to another" in {
    val account = "move_account"
    client.move("", account, Bitcoins(1)).flatMap { success =>
      assert(success)
      client.listAccounts().map { map =>
        assert(map(account) == Bitcoins(1))
      }
    }
  }
}
