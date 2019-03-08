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
import org.bitcoins.rpc.BitcoindRpcTestUtil
import org.bitcoins.rpc.client.v16.BitcoindV16RpcClient
import org.bitcoins.rpc.util.AsyncUtil
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}
import org.slf4j.Logger

import scala.async.Async.{async, await}
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Properties

class BitcoindV16RpcClientTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem(
    "BitcoindV16RpcClientTest_ActorSystem",
    BitcoindRpcTestUtil.AKKA_CONFIG)
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  val logger: Logger = BitcoinSLogger.logger

  private val accum: mutable.Builder[
    BitcoindV16RpcClient,
    Vector[BitcoindV16RpcClient]] = Vector.newBuilder[BitcoindV16RpcClient]

  lazy val clientsF: Future[(BitcoindV16RpcClient, BitcoindV16RpcClient)] =
    BitcoindRpcTestUtil.createNodePairV16(accum)

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(accum.result)

    TestKit.shutdownActorSystem(system)
  }

  behavior of "BitoindV16RpcClient"

  it should "be able to sign a raw transaction" in {
    for {
      (client, otherClient) <- clientsF
      addr <- client.getNewAddress
      _ <- otherClient.sendToAddress(addr, Bitcoins.one)
      _ <- otherClient.generate(6)
      peers <- client.getPeerInfo
      _ = {
        assert(peers.exists(_.networkInfo.addr == otherClient.getDaemon.uri))
      }
      recentBlock <- otherClient.getBestBlockHash
      _ <- AsyncUtil.retryUntilSatisfiedF(
        () => BitcoindRpcTestUtil.hasSeenBlock(client, recentBlock),
        1.second)
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
    for {
      (client, otherClient) <- clientsF
      address <- otherClient.getNewAddress
      txid <- client.sendFrom("", address, Bitcoins(1))
      transaction <- client.getTransaction(txid)
    } yield {
      assert(transaction.amount == Bitcoins(-1))
      assert(transaction.details.head.address.contains(address))
    }
  }

  it should "be able to get the amount received by an account and list amounts received by all accounts" in async {
    val (client, otherClient) = await(clientsF)

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
    for {
      (client, _) <- clientsF
      address <- client.getNewAddress(account1)
      acc1 <- client.getAccount(address)
      _ <- client.setAccount(address, account2)
      acc2 <- client.getAccount(address)
    } yield {
      assert(acc1 == account1)
      assert(acc2 == account2)
    }
  }

  it should "be able to get all addresses belonging to an account" in {
    for {
      (client, _) <- clientsF
      address <- client.getNewAddress
      addresses <- client.getAddressesByAccount("")
    } yield assert(addresses.contains(address))
  }

  it should "be able to get an account's address" in {
    val account = "a_new_account"
    for {
      (client, _) <- clientsF
      address <- client.getAccountAddress(account)
      result <- client.getAccount(address)
    } yield assert(result == account)
  }

  it should "be able to move funds from one account to another" in {
    val account = "move_account"
    for {
      (client, _) <- clientsF
      success <- client.move("", account, Bitcoins(1))
      map <- client.listAccounts()
    } yield {
      assert(success)
      assert(map(account) == Bitcoins(1))
    }
  }
}
