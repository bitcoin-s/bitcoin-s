package org.bitcoins.rpc.v16

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction.{
  TransactionConstants,
  TransactionInput,
  TransactionOutPoint
}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.{BitcoindRpcTestConfig, BitcoindRpcTestUtil}
import org.bitcoins.rpc.client.common.RpcOpts.AddNodeArgument
import org.bitcoins.rpc.client.v16.BitcoindV16RpcClient
import org.bitcoins.rpc.util.AsyncUtil
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}
import org.slf4j.Logger

import scala.async.Async.{async, await}
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.DurationInt
import scala.util.Properties

class BitcoindV16RpcClientTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem(
    "BitcoindV16RpcClientTest_ActorSystem")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  val logger: Logger = BitcoinSLogger.logger

  val client = new BitcoindV16RpcClient(BitcoindRpcTestUtil.instance())
  val otherClient = new BitcoindV16RpcClient(BitcoindRpcTestUtil.instance())

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

  behavior of "BitoindV16RpcClient"

  it should "be able to sign a raw transaction" in {
    for {
      addr <- client.getNewAddress
      _ <- otherClient.sendToAddress(addr, Bitcoins.one)
      _ <- otherClient.generate(6)
      peers <- client.getPeerInfo
      _ = {
        assert(peers.exists(_.networkInfo.addr == otherClient.getDaemon.uri))
      }
      recentBlock <- otherClient.getBestBlockHash
      _ <- AsyncUtil.retryUntilSatisfiedF(() =>
        BitcoindRpcTestUtil.hasSeenBlock(client, recentBlock), 1.second)
      (utxoTxid, utxoVout) <- client.listUnspent
        .map(_.filter(_.address.contains(addr)))
        .map(_.head)
        .map(utxo => (utxo.txid, utxo.vout))
      newAddress <- client.getNewAddress
      rawTx <- {
        val outPoint = TransactionOutPoint(utxoTxid.flip, UInt32(utxoVout))
        val input = TransactionInput(outPoint,
                                     ScriptSignature.empty,
                                     TransactionConstants.sequence)
        val outputs = Map(newAddress -> Bitcoins(0.5))

        client.createRawTransaction(Vector(input), outputs)
      }
      signedRawTx <- client.signRawTransaction(rawTx)
    } yield {
      assert(signedRawTx.complete)
    }
  }

  it should "be able to sign a raw transaction with a private key" ignore {
    ???
  }

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

  it should "be able to get the amount received by an account and list amounts received by all accounts" in async {
    val ourAccount = "another_new_account"
    val emptyAccount = "empty_account"

    val ourAccountAddress = await(client.getNewAddress(ourAccount))
    await(BitcoindRpcTestUtil
      .fundBlockChainTransaction(otherClient, ourAccountAddress, Bitcoins(1.5)))

    val accountlessAddress = await(client.getNewAddress)

    val sendAmt = Bitcoins(1.5)

    val _ = await(
      BitcoindRpcTestUtil
        .fundBlockChainTransaction(otherClient, accountlessAddress, sendAmt))

    if (Properties.isMac) Thread.sleep(10000)
    val ourAccountAmount = await(client.getReceivedByAccount(ourAccount))

    assert(ourAccountAmount == sendAmt)

    val receivedByAccount = await(client.listReceivedByAccount())

    val ourAccountOpt =
      receivedByAccount
        .find(_.account == ourAccount)

    assert(ourAccountOpt.isDefined)
    assert(ourAccountOpt.get.amount == Bitcoins(1.5))

    val accountLessOpt =
      receivedByAccount
        .find(_.account == "")

    assert(accountLessOpt.isDefined)
    assert(accountLessOpt.get.amount > Bitcoins(0))
    assert(!receivedByAccount.exists(_.account == emptyAccount))

    val accounts = await(client.listAccounts())

    assert(accounts(ourAccount) == Bitcoins(1.5))
    assert(accounts("") > Bitcoins(0))
    assert(!accounts.keySet.contains(emptyAccount))
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
